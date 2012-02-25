$(function()
{
    $(".do_timebar").timebar();


    var myOptions = {
        center: new google.maps.LatLng(52.310550, 4.957151),
        zoom: 13,
        mapTypeId: google.maps.MapTypeId.ROADMAP
    };
    var map = new google.maps.Map(document.getElementById("map"),
                                  myOptions);
    
    function loadData(timebar)
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
                    console.log(r.response.numFound);

                }
               });
    }

    Util.PubSub.subscribe('timebar-change', function(timebar) {
                              loadData(timebar);
                          });


});