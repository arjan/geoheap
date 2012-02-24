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
    

    Util.PubSub.subscribe('timebar-change', function(timebar) {
                              var args = {'from': Util.ISODateString(timebar.timeLeft), 
                                          'to': Util.ISODateString(timebar.timeRight),
                                          'start': Util.ISODateString(timebar.timeStart),
                                          'end': Util.ISODateString(timebar.timeEnd)
                                         };

                              $.ajax({url: '/query?' + $.param(args)});
                          });


});