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

    var allmarkers = {};
    
    function loadData()
    {
        var b = map.getBounds();
        
        var c = b.getCenter();
        var r = google.maps.geometry.spherical.computeDistanceBetween(b.getSouthWest(), c);

        var args = {'from': Util.ISODateString(timebar.timeLeft), 
                    'to': Util.ISODateString(timebar.timeRight),
                    'start': Util.ISODateString(timebar.timeStart),
                    'end': Util.ISODateString(timebar.timeEnd),
                    'lat': c.lat(),
                    'lon': c.lng(),
                    'radius': Math.ceil(r/1000)
                   };

        $.ajax({url: '/query?' + $.param(args),
                success: function(r) {
                    var data = r.facet_counts.facet_dates.date;
                    delete data.gap; delete data.start; delete data.end;
                    timebar.setData(r.facet_counts.facet_dates.date);

                    var hit = {};

                    $(r.response.docs).each(function() {
                                                var el = this;
                                                hit[el.id] = true;
                                                if (el.id in allmarkers) {
                                                    allmarkers[el.id].setVisible(true);
                                                    return;
                                                }
                                                var s = el.location.split(",");
                                                var pos = new google.maps.LatLng(parseFloat(s[0]), parseFloat(s[1]));
                                                var m = new google.maps.Marker({
                                                                                   position: pos,
                                                                                   title: el.screenname + ": " + el.text
                                                                               });
                                                allmarkers[el.id] = m;
                                                m.setMap(map);
                                            });
                    for (var id in allmarkers) if (!hit[id]) allmarkers[id].setVisible(false);
                }
               });
    }

    var mapChangeTimer = Util.IdleTimer(500, loadData);
    google.maps.event.addListener(map, 'bounds_changed', function() {
                                      mapChangeTimer.bump();
                                  });

    Util.PubSub.subscribe('timebar-change', function(timebar) {
                              loadData(timebar);
                          });


});