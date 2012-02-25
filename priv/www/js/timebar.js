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
            self.timeEnd = new Date((new Date()).getTime()+1000*3600);

            self.zoomlevel = 1;
            self.zoom = 1.0;
            self.pan = 0.0;
            
            self.inner = $("<div>").addClass("inner").appendTo(self.element);
                
            self.sparkline = $("<div>").appendTo(self.inner);


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
            var w = self.element.width();
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
            var w = self.inner.width();
            var d = self.bracketLeft.width()/w*.25;
            left = Math.max(d, Math.min(1-d, left));
            right = Math.max(d, Math.min(1-d, right));

            self.curtainLeft.css({width: left * w});
            self.bracketLeft.css({left: left * w - self.bracketLeft.width()});
            self.curtainRight.css({left: right * w, width: w - right * w});
            self.bracketRight.css({left: right * w});
            
            self.bracket = [left, right];
            self.bracketTimer.bump();

            var date = function(x) { return new Date(self.timeStart.getTime() + x * (self.timeEnd.getTime()-self.timeStart.getTime())); };
            self.timeLeft = date(left);
            self.legendLeft.css({right: w - left*w}).html(Util.timebarDate(self.timeLeft));
            self.timeRight = date(right);
            self.legendRight.css({left: right*w}).html(Util.timebarDate(self.timeRight));
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
                               $(document)
                                   .bind('mousemove.timebar', function(e) {

                                             var w = self.element.width();
                                             var dx = (e.pageX - ox)/w;

                                             //var w = self.element.width();
                                             //var totalw = w * self.zoom - w;
                                             //self.setPan(self.pan - 1/totalw * (e.pageX - ox));
                                             self.setBracket(self.bracket[0]+dx, self.bracket[1]+dx);
                                             ox = e.pageX;
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

                               $(document)
                                   .bind('mousemove.timebar', function(e) {
                                             var w = self.element.width();
                                             var dx = (e.pageX - ox)/w;
                                             self.setBracket(self.bracket[0]+dx, self.bracket[1], true);
                                             ox = e.pageX;
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
                               $(document)
                                   .bind('mousemove.timebar', function(e) {
                                             var w = self.element.width();
                                             var dx = (e.pageX - ox)/w;
                                             self.setBracket(self.bracket[0], self.bracket[1]+dx, false);
                                             ox = e.pageX;
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

            $("<span>")
                .html(Util.timebarDate(self.timeStart)).appendTo(self.legend);
            $("<span>")
                .addClass("right")
                .css({right: 0})
                .html(Util.timebarDate(self.timeEnd)).appendTo(self.legend);

            self.legendLeft = $("<span>")
                .addClass("right")
                .html("xx").appendTo(self.legend);
            self.legendRight = $("<span>")
                .html("xx").appendTo(self.legend);

        }
});
})(jQuery);