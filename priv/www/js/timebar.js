(function()
{
    $.widget("ui.timebar",
    {
        _init: function() {
            var self = this;

            self.barHeight = 60;
            self.subHeight = 20;

            self.data = [];
            for (var i=0; i<300; i++)
                self.data.push(100*Math.random());

            self.zoomlevel = 1;
            self.zoom = 1.0;
            self.pan = 0.0;
            
            self.inner = $("<div>").addClass("inner").appendTo(self.element);
                
            self.sparkline = $("<div>").appendTo(self.inner);

            self.curtainLeft = $("<div>").addClass("curtain").appendTo(self.inner);
            self.curtainRight = $("<div>").addClass("curtain").appendTo(self.inner);
            self.bracketLeft = $("<img>").attr("src", "/img/bracket-left.png").addClass("bracket bracket-left").appendTo(self.inner);
            self.bracketRight = $("<img>").attr("src", "/img/bracket-right.png").addClass("bracket bracket-right").appendTo(self.inner);

            self.bracketTimer = self.idleTimer(500, self.bracketChanged);
            self.setBracket(0.2, 0.8);

            self.refresh();
            self.setupDrag();
        },

        refresh: function() {
            var self = this;
            var w = self.element.width();
            self.sparkline.empty().sparkline(self.data, {height: self.barHeight, width: self.zoom * w});
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

            self.bracket = [Math.max(d, Math.min(1-d, left)), Math.max(d, Math.min(1-d, right))];

            self.curtainLeft.css({width: left * w});
            self.bracketLeft.css({left: left * w - self.bracketLeft.width()});
            self.curtainRight.css({left: right * w, width: w - right * w});
            self.bracketRight.css({left: right * w});
            self.bracketTimer.bump();
        },

        bracketChanged: function() {
            console.log(1111);
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


        idleTimer: function (d, c)
        {
            return (function(delay, callback) {
                        var timeout = null;
                        var self = this;
                        self.callback = callback;

                        self.cancel = function ()
                        {
                            if (timeout)
                            {
                                clearTimeout(timeout);
                                timeout = null;
                            }
                        };
                        self.bump = function ()
                        {
                            self.cancel();
                            timeout = setTimeout(function () {self.callback(); timeout = null; }, delay);
                        };
                        return self;
            })(d, c);
        }
    }
);
})(jQuery);