/**
 * Map functionality. This file should be refactored, currently it is
 * a bunch of functionality thrown together without much coherence.
 */

$(function()
{
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

    function showInfoWindow(el)
    {
        var fullname = function(item, el) {
            if (!item) return el.screenname;
            if (item.source == 'instagram')
                return item.original.user.full_name;
            if (item.source == 'twitter')
                return item.original.user.name;
        };
        var showitem = function(item) {
            var html = "";
            html += "<p>" + fullname(item, el) + " @ " + el.date + "</p>";
            if (el.thumbnail) html +="<p style=\"height:150px\"><img src=\""+el.thumbnail+"\" width=\"150\" height=\"150\" /></p>";
            if (el.text) html +="<p><strong>"+el.text+"</strong></p>";
            infowindow.setContent(html);
            infowindow.setPosition(el.pos);
            infowindow.open(map);
        };
        if (!el.store_id) {
            showitem(null, el);
            return;
        }
        $.ajax({url: '/item?id=' + encodeURIComponent(el.store_id),
                success: showitem
               });
    }
    google.maps.event.addListener(map, 'click', function(){infowindow.close();});

    var allmarkers = {};
    var loading = false;


    function loadData()
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
        var args = {'from': Util.ISODateString(timebar.timeLeft), 
                    'to': Util.ISODateString(timebar.timeRight),
                    'start': Util.ISODateString(timebar.timeStart),
                    'end': Util.ISODateString(timebar.timeEnd),
                    'lat': c.lat(),
                    'lon': c.lng(),
                    'radius': Math.ceil(r/1000),
                    'q': sources + txt
                   };

        $.ajax({url: '/query?' + $.param(args),
                success: function(r) {
                    var html = "";
                    html += "<div>Results: " + r.response.numFound + "</div>";
                    $("#stats").html(html);

                    var data = r.facet_counts.facet_dates.date;
                    delete data.gap; delete data.start; delete data.end;
                    timebar.setData(r.facet_counts.facet_dates.date);

                    var hit = {};
                    var numnew = 0;
                    var lastnew = null;
                    var lastnewdist = null;

                    $(r.response.docs).each(function() {
                                                var el = this;
                                                hit[el.id] = true;
                                                if (el.id in allmarkers) {
                                                    allmarkers[el.id].setVisible(true);
                                                    return;
                                                }
                                                var s = el.location.split(",");
                                                el.pos = new google.maps.LatLng(parseFloat(s[0]), parseFloat(s[1]));
                                                var icon = el.source == "twitter" ? "/img/blue.png" : "/img/red.png";
                                                var m = new google.maps.Marker({
                                                                                   position: el.pos,
                                                                                   title: el.screenname + " @ " + el.date + ": " + el.text,
                                                                                   icon: icon
                                                                               });
                                                google.maps.event.addListener(m, 'click', function(){showInfoWindow(el);});

                                                numnew++;
                                                if (numnew < 20) {
                                                    m.setAnimation(google.maps.Animation.DROP);
                                                    var r = google.maps.geometry.spherical.computeDistanceBetween(el.pos, c);
                                                    if (lastnewdist === null || r < lastnewdist)
                                                        lastnew = el;
                                                }

                                                allmarkers[el.id] = m;
                                                m.setMap(map);
                                            });
                    for (var id in allmarkers) if (!hit[id]) allmarkers[id].setVisible(false);
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
                        loadData();
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