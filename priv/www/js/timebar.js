(function()
{
    $.widget("ui.timebar",
    {
        _init: function() {
            var self = this;

            self.barHeight = 60;
            self.subHeight = 20;

            self.data = [];

            var now = (new Date()).getTime()/1000;
            now = (now - now % 3600) + 3600;

            self.timeStartMaximum = new Date((now-3600*48*1)*1000);
            self.timeStartMinimum = new Date((now-3600*24*60)*1000);

            self.timeStart = new Date((now-3600*24*7)*1000);
            self.timeEnd = new Date(now*1000);

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
            self.setDateBracket(new Date((now-4*3600)*1000), self.timeEnd);
            self.refresh();
            self.setupDrag();
        },

        getTimeStart: function() {
            var self = this;
            return self.timeStart;
        },

        setTimeStart: function(d) {
            self.timeStart = d;
            self.refresh();
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

            // Set end time to next hour
            var now = (new Date()).getTime()/1000;
            now = (now - now % 3600) + 3600;
            self.timeEnd = new Date(now*1000);
            self.setBracket(self.bracket[0], self.bracket[1]); // update bracket times

            var w = self.element.width() - 2*self.bracketLeft.width();

            self.sparkline.empty().sparkline(self.data, {
                                                 type: 'bar',
                                                 barWidth: (self.zoom*w)/self.data.length,
                                                 barSpacing: 0,
                                                 barColor: 'green',
                                                 height: self.barHeight, width: self.zoom * w});
            //self.curtainLeft.css({width: self.bracket[0] * self.zoom * w});
            var rw = (1-self.bracket[1]) * self.zoom * w;
            //self.curtainRight.css({width: rw, left: self.zoom * w - rw});
        },

        getBracket: function() {
            var self = this;
            return self.bracket;
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

            var n = (self.timeEnd.getTime()-self.timeStart.getTime()) / (1000*900); // 15 minute increment
            if (left > 0.05)
                left = Math.floor(left*n)/n;
            right = Math.floor(right*n)/n;

            self.curtainLeft.css({width: d+left * w});
            self.bracketLeft.css({left: d+left * w - self.bracketLeft.width()});
            self.curtainRight.css({left: d+right * w, width: w - right * w});
            self.bracketRight.css({left: d+right * w});
            
            self.bracket = [left, right];

            var date = function(x) { return new Date(self.timeStart.getTime() + x * (self.timeEnd.getTime()-self.timeStart.getTime())); };
            self.timeLeft = date(left);
            self.timeRight = date(right);

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

        /** 
         * Adds an amount of seconds to the start time. Returns
           whether or not the time has changed (e.g. if it is still
           within the min/max boundaries) */
        addToStartTime: function(seconds) {
            var self = this, original = self.timeStart.getTime();
            self.timeStart = new Date(Math.max(self.timeStartMinimum, Math.min(self.timeStartMaximum, self.timeStart.getTime()+1000*seconds)));
            return original != self.timeStart.getTime();
        },
        
        setDateBracket: function(start, end) {
            var self = this;

            start = new Date(Math.max(self.timeStartMinimum, Math.min(self.timeStartMaximum, start)));
            if (start <= self.timeStart) {
                self.timeStart = start;
            }

            var d = self.timeEnd.getTime() - self.timeStart.getTime();
            var right = (end.getTime()-self.timeStart.getTime())/d;
            var left = (start.getTime()-self.timeStart.getTime())/d;
            self.setBracket(left, right);
        },
        
        setupDrag: function() {
            var self = this;

            self.element            
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
                                             self.bracketTimer.bump();
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

                               var zoomInterval = null;
                               function clearZoomInterval() {
                                   clearInterval(zoomInterval);
                                   zoomInterval = null;
                               }
                               var lastUpdate = new Date();
                               
                               $(document)
                                   .bind('mousemove.timebar', function(e) {
                                             var w = self.element.width();
                                             var b = start + (e.pageX - ox)/w;

                                             // dragging in the leftmost region will cause the timebar to zoom out
                                             if (b <= 0.05) {
                                                 if (zoomInterval) clearZoomInterval();
                                                 zoomInterval = setInterval(function() {
                                                                                if (!self.addToStartTime(-3600)) return;
                                                                                self.setBracket(0.05, self.bracket[1]);
                                                                                self.data.unshift(0); // add temporary zero datapoint
                                                                                self.refresh();
                                                                                if ((new Date()) - lastUpdate > 2000) {
                                                                                    self.bracketTimer.bump();
                                                                                    lastUpdate = new Date();
                                                                                }
                                                                            }, 50);
                                             } else if (0.45 < b && b < 0.55) {
                                                 if (zoomInterval) clearZoomInterval();
                                                 zoomInterval = setInterval(function() {
                                                                                if (!self.addToStartTime(3600)) return;
                                                                                self.setBracket(0.5, self.bracket[1]);
                                                                                self.data.shift(); // remove a datapoint
                                                                                self.refresh();
                                                                            }, 50);

                                             } else {
                                                 clearZoomInterval();
                                                 self.setBracket(b, self.bracket[1], true);
                                                 self.bracketTimer.bump();
                                             }

                                         })
                                   .bind('mouseup.timebar', function(e) {
                                             clearZoomInterval();
                                             $(this).unbind('mousemove.timebar');
                                             $(this).unbind('mouseup.timebar');
                                             self.dragging = false;                               
                                             self.bracketTimer.bump();
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
                                             self.bracketTimer.bump();
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
            self.legendLeft = $("<span>")
                .html("xx").appendTo(self.legend);
            self.legendRight = $("<span>")
                .html("xx").appendTo(self.legend);

        }
});
})(jQuery);