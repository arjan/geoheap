(function()
{
    $.widget("ui.timebar",
    {
        _init: function() {
            var self = this;

            self.barHeight = 60;
            self.subHeight = 20;

            self.data = [];
//            for (var i=0; i<300; i++)
//                self.data.push(100*Math.random());
            self.timeStart = new Date("2012-02-22");
            var now = (new Date()).getTime()/1000;
            now = (now - now % 3600) + 3600;
            self.timeEnd = new Date(now*1000);
console.log(self.timeEnd.toTimeString());


            self.zoomlevel = 1;
            self.zoom = 1.0;
            self.pan = 0.0;
            
            self.inner = $("<div>").addClass("inner").appendTo(self.element);
                
            self.sparkline = $("<div>").addClass("bars").appendTo(self.inner);


            self.curtainLeft = $("<div>").addClass("curtain").appendTo(self.inner);
            self.curtainRight = $("<div>").addClass("curtain").appendTo(self.inner);
            self.bracketLeft = $("<img>").attr("src", "/img/bracket-left.png").addClass("bracket bracket-left").appendTo(self.inner);
            self.bracketRight = $("<img>").attr("src", "/img/bracket-right.png").addClass("bracket bracket-right").appendTo(self.inner);

            self.setupLegend();

            self.bracketTimer = Util.IdleTimer(200, self.bracketChanged, self);
            self.setBracket(0.0, 1.0);

            self.refresh();
            self.setupDrag();
        },

        setData: function(data) {
            var self = this;
            self.data = [];
            for (var k in data)
                self.data.push(data[k]);
            self.refresh();
        },

        refresh: function() {
            var self = this;
            var w = self.element.width() - 2*self.bracketLeft.width();

            //self.sparkline.empty().sparkline(self.data, {height: self.barHeight, width: self.zoom * w});
            self.sparkline.empty().sparkline(self.data, {
                                                 type: 'bar',
                                                 barWidth: (self.zoom*w)/self.data.length - 1,
                                                 barSpacing: 1,
                                                 height: self.barHeight, width: self.zoom * w});
            //self.curtainLeft.css({width: self.bracket[0] * self.zoom * w});
            var rw = (1-self.bracket[1]) * self.zoom * w;
            //self.curtainRight.css({width: rw, left: self.zoom * w - rw});
        },

        setBracket: function(left, right, leftMoving) {
            var self = this;            
            
            if (left > right) {
                if (leftMoving) 
                    right = left;
                else
                    left = right;
            }
            var d = self.bracketLeft.width();
            var w = self.inner.width()-2*d;

            left = Math.max(0, Math.min(1, left));
            right = Math.max(0, Math.min(1, right));

            var n = (self.timeEnd.getTime()-self.timeStart.getTime()) / (1000*900);
            left = Math.floor(left*n)/n;
            right = Math.floor(right*n)/n;

            self.curtainLeft.css({width: d+left * w});
            self.bracketLeft.css({left: d+left * w - self.bracketLeft.width()});
            self.curtainRight.css({left: d+right * w, width: w - right * w});
            self.bracketRight.css({left: d+right * w});
            
            self.bracket = [left, right];
            self.bracketTimer.bump();

            var date = function(x) { return new Date(self.timeStart.getTime() + x * (self.timeEnd.getTime()-self.timeStart.getTime())); };
            self.timeLeft = date(left);
            self.legendLeft.html(Util.timebarDate(self.timeLeft));
            if (left < 0.075) {
                self.legendLeft
                    .css({left: d})
                    .removeClass("right"); 
            } else {
                self.legendLeft
                    .css({right: Math.max(d+w-left*w, d+0.075*w)})
                    .addClass("right"); 
            }
            self.timeRight = date(right);
            self.legendRight.html(Util.timebarDate(self.timeRight));
            if (right > 0.925) {
                self.legendRight
                    .css({right: d})            
                    .addClass("right"); 
            } else {
                self.legendRight
                    .css({left: d+right*w})            
                    .removeClass("right"); 
            }
        },

        bracketChanged: function() {
            var self = this;
            Util.PubSub.publish('timebar-change', [self]);
        },

        setPan: function(pan) {
            var self = this;            
            self.pan = Math.max(0, Math.min(1, pan));

            var w = self.element.width();
            var totalw = w * self.zoom - w;
            self.inner.css({left: -Math.floor(totalw * self.pan)+"px"});
        },

        setZoom: function(level, dx) {
            var self = this;
            level = Math.max(1, Math.min(10, level));
            if (level == self.zoomlevel) return;

            var px = (-parseInt(self.inner.css('left'))+dx)/self.zoom;
            self.zoomlevel = level;
            
            self.zoom = 0;
            for (var i=1; i<= self.zoomlevel; i++) self.zoom+= 1/i + (i-1)/i;

            px = px * self.zoom - dx;
            var w = self.element.width();
            var totalw = w * self.zoom - w;

            self.refresh();
            self.setPan(1/totalw * px);
        },
        
        setupDrag: function() {
            var self = this;

            self.element            
                .mousewheel(function(e, delta) {
                                self.setZoom(self.zoomlevel + delta, e.pageX - self.element.offset().left);
                            })
                .mousedown(function(e) {
                               e.preventDefault();
                               if (self.dragging) return;
                               var ox = e.pageX;
                               self.dragging = true;                               
                               var start = self.bracket[0];
                               var end = self.bracket[1];
                               $(document)
                                   .bind('mousemove.timebar', function(e) {

                                             var w = self.element.width();
                                             var b1 = start + (e.pageX - ox)/w;
                                             var b2 = end + (e.pageX - ox)/w;
                                             self.setBracket(b1, b2);
                                         })
                                   .bind('mouseup.timebar', function() {
                                             $(this).unbind('mousemove.timebar');
                                             $(this).unbind('mouseup.timebar');
                                             self.dragging = false;                               
                                         });
                           });

            self.bracketLeft
                .mousedown(function(e) {
                               e.preventDefault();
                               if (self.dragging) return;
                               var ox = e.pageX;
                               self.dragging = true;           
                               var start = self.bracket[0];
                               $(document)
                                   .bind('mousemove.timebar', function(e) {
                                             var w = self.element.width();
                                             var b = start + (e.pageX - ox)/w;
                                             self.setBracket(b, self.bracket[1], true);
                                         })
                                   .bind('mouseup.timebar', function(e) {
                                             $(this).unbind('mousemove.timebar');
                                             $(this).unbind('mouseup.timebar');
                                             self.dragging = false;                               
                                         });
                           });
            self.bracketRight
                .mousedown(function(e) {
                               e.preventDefault();
                               if (self.dragging) return;
                               var ox = e.pageX;
                               self.dragging = true;
                               var start = self.bracket[1];
                               $(document)
                                   .bind('mousemove.timebar', function(e) {
                                             var w = self.element.width();
                                             var b = start + (e.pageX - ox)/w;
                                             self.setBracket(self.bracket[0], b, false);
                                         })
                                   .bind('mouseup.timebar', function(e) {
                                             $(this).unbind('mousemove.timebar');
                                             $(this).unbind('mouseup.timebar');
                                             self.dragging = false;
                                         });
                           });
        },

        setupLegend: function() {
            var self = this;
            self.legend = $("<div>").addClass("legend").appendTo(self.inner);
            /*
            $("<span>")
                .html(Util.timebarDate(self.timeStart)).appendTo(self.legend);
            $("<span>")
                .addClass("right")
                .css({right: 0})
                .html(Util.timebarDate(self.timeEnd)).appendTo(self.legend);
             */
            self.legendLeft = $("<span>")
                .html("xx").appendTo(self.legend);
            self.legendRight = $("<span>")
                .html("xx").appendTo(self.legend);

        }
});
})(jQuery);