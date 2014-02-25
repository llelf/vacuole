
(function ()
{

var desc = $('.wut .desc');

$('.wut .toggle').click (function () { desc.toggle() });

$('.wut .examples').each (function (i,e)
			  {
			    $(e).click(function (r,o,q) {
			      console.log($(this).text()) });
			  });
  


})();
