/**
 * Map functionality. This file should be refactored, currently it is
 * a bunch of functionality thrown together without much coherence.
 */

function touchHandler(event)
{
    var touches = event.changedTouches,
        first = touches[0],
        type = "";

         switch(event.type)
    {
        case "touchstart": type = "mousedown"; break;
        case "touchmove":  type="mousemove"; break;        
        case "touchend":   type="mouseup"; break;
        default: return;
    }

    var simulatedEvent = document.createEvent("MouseEvent");
    simulatedEvent.initMouseEvent(type, true, true, window, 1,
                              first.screenX, first.screenY,
                              first.clientX, first.clientY, false,
                              false, false, false, 0/*left*/, null);
    first.target.dispatchEvent(simulatedEvent);
}

document.addEventListener("touchstart", touchHandler, true);
document.addEventListener("touchmove", touchHandler, true);
document.addEventListener("touchend", touchHandler, true);
document.addEventListener("touchcancel", touchHandler, true);    


$(function()
{
    var filtersShowing = true;

    function showFilterTool() {
        filtersShowing = true;
        $(".filtertool-contents").show();
        $("#apply-btn").show();
        $("#hide-btn").text("Hide");
    }

    function hideFilterTool() {
        $(".filtertool-contents").hide();
        $("#apply-btn").hide();
        $("#hide-btn").text("Show");
        filtersShowing = false;
    }

    $("#hide-btn").click(
        function() 
        {
            filtersShowing ? hideFilterTool() : showFilterTool();
        });
    
    var allSources = ['twitter', 'instagram', 'vbdb'];

    var hashToState = function(hash) {
        return JSON.parse(decodeURIComponent(hash.substr(1)));
    };
    var stateToHash = function(state) {
        return encodeURIComponent(JSON.stringify(state));
    };

    var ItemMapping = {
        'twitter': function(item) 
        {
            this.fullName = function() {
                return this.data.original.user.name;
            };
            this.userURL = function() {
                return "http://www.twitter.com/#!/" + this.data.original.user.screen_name;
            };
            this.markerImage = function() {
                return "/img/blue.png";
            };
            return this;
        },
        'instagram': function() {
            this.fullName = function() {
                return this.data.original.user.full_name;
            };
            this.userURL = function() {
                return this.data.original.link;
            };
            this.markerImage = function() {
                return "/img/red.png";
            };
            return this;
        },
        'vbdb': function() {
            this.fullName = function() {
                return 'Verbeter de Buurt';
            };
            this.userURL = function() {
                return "";
            };
            this.markerImage = function() {
                return "/img/green.png";
            };
            return this;
        }
    };
    var ItemFactory = function(item) {
        var inst = new ItemMapping[item.source]();
        inst.data = item;
        var s = item.location.split(",");
        inst.latlng = new google.maps.LatLng(parseFloat(s[0]), parseFloat(s[1]));
        inst.marker = new google.maps.Marker({position: inst.latlng, icon: inst.markerImage()});
        return inst;
    };

    
    $(".do_timebar:first").timebar();
    var timebar = $(".do_timebar:first").data('timebar');

    var myOptions = {
        center: new google.maps.LatLng(52.310550, 4.957151),
        zoom: 13,
        mapTypeControlOptions: {
            mapTypeIds: ['Floriade Radar', google.maps.MapTypeId.SATELLITE]
        }
    };
    var map = new google.maps.Map(document.getElementById("map"),
                                  myOptions);

    map.mapTypes.set('Floriade Radar', window.FloriadeRadarStyle);
    map.setMapTypeId('Floriade Radar');

    var getState = function() {
        var c = map.getCenter();
        var sources = [];
        $("input.source:checked").each(function() { sources.push($(this).attr("id").substr(4));});
        return [c.lat(), c.lng(), map.getZoom(),sources, view, timebar.getBracket(), Util.ISODateString(timebar.getTimeStart())];
    };
    var applyState = function(state) {
        map.setCenter(new google.maps.LatLng(state[0], state[1]));
        map.setZoom(state[2]);
        $("input.source").each(function(){$(this).removeAttr("checked"); if ($.inArray($(this).attr("id").substr(4), state[3])>=0) $(this).attr("checked", "checked");});
        view = state[4];
        $("input.view").removeAttr("checked");
        $("#view-" + view).attr("checked", "checked");
//FIXME        timebar.setTimeStart(new Date(state[6]));
        timebar.setBracket(state[5][0], state[5][1]);
    };

    var allItems = {};
    var loading = false;
    var lastLoad = null;
    var lastNumResults = 0;
    var view = 'items';

    if (document.location.hash) {
        try {
            var state = hashToState(document.location.hash);
            applyState(state);
        } catch (x) {
        }
    }
    
    function linkURLS(text) {
        var linkWithProtocol = /(\b(https?|ftp):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;]*[-A-Z0-9+&@#\/%=~_|])/gim;
        text = text.replace(linkWithProtocol, '<a href="$1" target="_blank">$1</a>');
        
        var linkWithoutProtocol = /(^|[^\/])(www\.[\S]+(\b|$))/gim;
        text = text.replace(linkWithoutProtocol, '$1<a href="http://$2" target="_blank">$2</a>');
        
        var linkEmail = /(\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,6})/gim;
        text = text.replace(linkEmail, '<a href="mailto:$1">$1</a>');
        
        return text;
    }
    
    var infowindow = new google.maps.InfoWindow({disableAutoPan: true});

    function showInfoWindow(item)
    {
        var showitem = function(detail) {
            $.extend(item.data, detail);
            var html = "";
            html += "<div class=\"infowindow-container\">";
            html += "<div class=\"infowindow-header\"><a href=\"" + item.userURL() + "\" target=\"_blank\">" + item.fullName() + "</a>";
            html += "<div class=\"infowindow-timestamp\">" + Util.FriendlyDateString(new Date(detail.date)) + "</div>";
            if (detail.thumbnail) html +="<img class=\"infowindow-image\" src=\""+detail.thumbnail+"\" width=\"150\" height=\"150\" />";
            if (detail.text) html +="<div class=\"infowindow-text\">"+linkURLS(detail.text)+"</div>";
            html += "</div>";
            infowindow.setContent(html);
            infowindow.setPosition(item.latlng);
            infowindow.open(map);
        };

        $.ajax({url: '/item?id=' + encodeURIComponent(item.data.id),
                success: showitem
               });
    }
    google.maps.event.addListener(map, 'click', function(){infowindow.close();});

    
    function loadData(incremental)
    {
        document.location = "#" + stateToHash(getState());
        
        if (loading) return;
        $("#loader").show();
        loading = true;

        var b = map.getBounds();
        
        var c = b.getCenter();
        var r = google.maps.geometry.spherical.computeDistanceBetween(
            new google.maps.LatLng(c.lat(), b.getSouthWest().lng()),
            c);

        var srci = $("#src-instagram:checked").length > 0;
        var srct = $("#src-twitter:checked").length > 0;
        
        var sources = "";
        $("input.source:checked").each(function() { sources += " source: " + $(this).attr("id").substr(4);});
        if (sources)
            sources = "+(" + sources + ")";

        var v = $("#q").val().trim();
        var txt = v ? (" +alltext:"+v) : "";
        var args = {'from': Util.ISODateString(incremental ? lastLoad : timebar.timeLeft), 
                    'to': Util.ISODateString(timebar.timeRight),
                    'start': Util.ISODateString(timebar.timeStart),
                    'end': Util.ISODateString(timebar.timeEnd),
                    'lat': c.lat(),
                    'lon': c.lng(),
                    'radius': Math.ceil(r/1000),
                    'facet': !incremental,
                    'q': sources + txt
                   };

        $.ajax({url: '/query?' + $.param(args),
                success: function(r) {
                    lastLoad = new Date();
                    lastNumResults = incremental ? (lastNumResults + r.response.numFound) : r.response.numFound;
                    var html = "";
                    html += "<div>Results: " + lastNumResults + "</div>";
                    $("#stats").html(html);

                    if (r.facet_counts) {
                        var data = r.facet_counts.facet_dates.date;
                        delete data.gap; delete data.start; delete data.end;
                        timebar.setData(r.facet_counts.facet_dates.date);
                    }

                    var hit = {};
                    var numnew = 0;
                    var lastnew = null;
                    var lastnewdist = null;

                    $(r.response.docs).each(function() {
                                                var el = this;
                                                hit[el.id] = true;
                                                if (el.id in allItems) {
                                                    if (view == 'items') allItems[el.id].marker.setVisible(true);
                                                    return;
                                                }
                                                var item = ItemFactory(el);
                                                google.maps.event.addListener(item.marker, 'click', function(){showInfoWindow(item);});
                                                numnew++;
                                                if (numnew < 2) {
                                                    item.marker.setAnimation(google.maps.Animation.DROP);
                                                    var r = google.maps.geometry.spherical.computeDistanceBetween(item.latlng, c);
                                                    if (lastnewdist === null || r < lastnewdist)
                                                        lastnew = item;
                                                }
                                                allItems[item.data.id] = item;
                                                if (view != 'items') allItems[el.id].marker.setVisible(false);
                                                item.marker.setMap(map);
                                            });
                    if (view == 'heatmap') {
                        for (var i in allItems) allItems[i].marker.setVisible(false);
                    }
                    hoverlay.heatmap.setVisible(view == 'heatmap');
                    if (!incremental) {
                        for (var id in allItems) if (!hit[id]) allItems[id].marker.setVisible(false);
                        if (view == 'heatmap') refreshHeatmap(r);
                    }
                    if (lastnew && view == 'items') 
                        showInfoWindow(lastnew);
                    $("#loader").hide();
                    loading = false;
                },
                error: function() {
                    $("#loader").hide();
                    loading = false;
                }
               });
    }

    // heatmap
    var hoverlay = new HeatmapOverlay(map, {"radius":50, "visible":true, "opacity":80});

    function refreshHeatmap(r) {
        var dataset = {data: [], max: 1};

        r = r.facet_counts.facet_fields.geohash_8;

        for (var i=0; i< r.length; i+= 2) {
            var hash = r[i].substr(1);
            var c = r[i+1];
            var h = decodeGeoHash(hash);

            var lat = h.latitude[2];
            var lng = h.longitude[2];

            dataset.data.push({lat: lat, lng: lng, count: c});
            dataset.max = Math.max(dataset.max, c);
        }
/*
        for (var id in allItems) {
            if (!allItems[id].marker.getVisible()) continue;
            var p = allItems[id].latlng;
            dataset.data.push({lat: p.lat(), lng: p.lng(), count: 1});
        }
*/
        hoverlay.setDataSet(dataset);
    }
    
    var mapChangeTimer = Util.IdleTimer(1000, loadData);
    google.maps.event.addListener(map, 'bounds_changed', function() {
                                      mapChangeTimer.bump();
                                  });

    Util.PubSub.subscribe('timebar-change', function(timebar) {
                              loadData(timebar);
                          });


    setInterval(function() {
                    if(timebar.timeRight > new Date()) {
                        // poll when we're in "realtime" mode
                        loadData(true);
                    }
                }, 5000);

    $("#filter-form").submit(
        function(e) {
            e.preventDefault();
            loadData();
        });

    var geocoder = new google.maps.Geocoder();

    $("#location-form").submit(
        function(e) {
            e.preventDefault();
            var address = $("#location").val();
            geocoder.geocode({
                                 address: address,
                                 region: 'nl'},
                             function(results, status) {
                                 if (status == google.maps.GeocoderStatus.OK) {
                                     map.setCenter(results[0].geometry.location);
                                 } else {
                                     alert("Geocode was not successful for the following reason: " + status);
                                 }
                            });
        });
    

    $("input.view").change(
        function() { 
            view = $("input.view:checked").val();
            mapChangeTimer.bump();
        });
});