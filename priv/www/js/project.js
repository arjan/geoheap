/**
 * Map functionality. This file should be refactored, currently it is
 * a bunch of functionality thrown together without much coherence.
 */

$(function()
{
    var ItemMapping = {
        'twitter': function(item) 
        {
            this.fullName = function() {
                return this.data.original.user.name;
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
            this.markerImage = function() {
                return "/img/red.png";
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
        mapTypeId: google.maps.MapTypeId.ROADMAP
    };
    var map = new google.maps.Map(document.getElementById("map"),
                                  myOptions);

    var infowindow = new google.maps.InfoWindow({disableAutoPan: true});

    function showInfoWindow(item)
    {
        var showitem = function(detail) {
            $.extend(item.data, detail);
            var html = "";
            html += "<p>" + item.fullName() + " @ " + detail.date + "</p>";
            if (detail.thumbnail) html +="<p style=\"height:150px\"><img src=\""+detail.thumbnail+"\" width=\"150\" height=\"150\" /></p>";
            if (detail.text) html +="<p><strong>"+detail.text+"</strong></p>";
            infowindow.setContent(html);
            infowindow.setPosition(item.latlng);
            infowindow.open(map);
        };

        $.ajax({url: '/item?id=' + encodeURIComponent(item.data.id),
                success: showitem
               });
    }
    google.maps.event.addListener(map, 'click', function(){infowindow.close();});

    var allItems = {};
    var loading = false;
    var lastLoad = null;
    var lastNumResults = 0;

    function loadData(incremental)
    {
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
        var sources = (srci != srct) ? ("+source:"+(srci ? "instagram":"twitter")+" ") : "";
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
                                                    allItems[el.id].marker.setVisible(true);
                                                    return;
                                                }
                                                var item = ItemFactory(el);
                                                google.maps.event.addListener(item.marker, 'click', function(){showInfoWindow(item);});
                                                numnew++;
                                                if (numnew < 20) {
                                                    item.marker.setAnimation(google.maps.Animation.DROP);
                                                    var r = google.maps.geometry.spherical.computeDistanceBetween(item.latlng, c);
                                                    if (lastnewdist === null || r < lastnewdist)
                                                        lastnew = item;
                                                }
                                                allItems[item.data.id] = item;
                                                item.marker.setMap(map);
                                            });
                    if (!incremental) {
                        for (var id in allItems) if (!hit[id]) allItems[id].marker.setVisible(false);
                    }
                    if (lastnew) 
                        showInfoWindow(lastnew);
                    $("#loader").hide();
                    loading = false;
                }
               });
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
    

});