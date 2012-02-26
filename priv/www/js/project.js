$(function()
{
    $(".do_timebar:first").timebar();
    var timebar = $(".do_timebar:first").data('timebar');

    var myOptions = {
        center: new google.maps.LatLng(52.310550, 4.957151),
        zoom: 13,
        mapTypeId: google.maps.MapTypeId.TERRAIN //ROADMAP
    };
    var map = new google.maps.Map(document.getElementById("map"),
                                  myOptions);

    var allmarkers = {};
    var loading = false;

    function loadData()
    {
        if (loading) return;
        loading = true;

        var b = map.getBounds();
        
        var c = b.getCenter();
        var r = google.maps.geometry.spherical.computeDistanceBetween(b.getSouthWest(), c);

        var args = {'from': Util.ISODateString(timebar.timeLeft), 
                    'to': Util.ISODateString(timebar.timeRight),
                    'start': Util.ISODateString(timebar.timeStart),
                    'end': Util.ISODateString(timebar.timeEnd),
                    'lat': c.lat(),
                    'lon': c.lng(),
                    'radius': Math.ceil(r/1000),
                    'q': $("#q").val()
                   };

        $.ajax({url: '/query?' + $.param(args),
                success: function(r) {
                    var html = "";
                    html += "<p>Results: " + r.response.numFound;
                    $("#stats").html(html);

                    var data = r.facet_counts.facet_dates.date;
                    delete data.gap; delete data.start; delete data.end;
                    timebar.setData(r.facet_counts.facet_dates.date);

                    var hit = {};
                    var numnew = 0;
                    $(r.response.docs).each(function() {
                                                var el = this;
                                                hit[el.id] = true;
                                                if (el.id in allmarkers) {
                                                    allmarkers[el.id].setVisible(true);
                                                    return;
                                                }
                                                var s = el.location.split(",");
                                                var pos = new google.maps.LatLng(parseFloat(s[0]), parseFloat(s[1]));
                                                var icon = el.source == "twitter" ? "/img/blue.png" : "/img/red.png";
                                                var m = new google.maps.Marker({
                                                                                   position: pos,
                                                                                   title: el.screenname + " @ " + el.date + ": " + el.text,
                                                                                   icon: icon
                                                                               });
                                                numnew++;
                                                if (numnew < 20)
                                                    m.setAnimation(google.maps.Animation.DROP);

                                                allmarkers[el.id] = m;
                                                m.setMap(map);
                                            });
                    for (var id in allmarkers) if (!hit[id]) allmarkers[id].setVisible(false);
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