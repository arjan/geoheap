Util = {
    timebarDate: function(d) {
        function pad(n){return n<10 ? '0'+n : n;}
        var m = ['jan', 'feb', 'maa', 'apr', 'mei', 'jun', 'jul', 'aug', 'sep', 'okt', 'nov', 'dec'];
        return d.getDate() + ' '
            + m[d.getMonth()] + ' '
            + d.getHours()+':'
            + pad(d.getMinutes());
    },
  
    ISODateString: function(d)
    {
        function pad(n){return n<10 ? '0'+n : n;}
        return d.getUTCFullYear()+'-'
            + pad(d.getUTCMonth()+1)+'-'
            + pad(d.getUTCDate())+'T'
            + pad(d.getUTCHours())+':'
            + pad(d.getUTCMinutes())+':'
            + pad(d.getUTCSeconds())+'Z';
    },

    PubSub:  {
        _subs: {},

        subscribe: function(node, cb) {
            if (!(node in Util.PubSub._subs))
                Util.PubSub._subs[node] = [];
            Util.PubSub._subs[node].push(cb);
        },

        publish: function(node, args) {
            if (!(node in Util.PubSub._subs))
                return;
            var all = Util.PubSub._subs[node];
            for (i =0; i<all.length; i++) {
                all[i].apply(undefined, args);
            }
        }
    },

    IdleTimer: function(delay, callback, context) {
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
            timeout = setTimeout(function () { callback.apply(context, []); timeout = null; }, delay);
        };
        return self;
    }
};
