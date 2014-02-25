
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



function initTerm (handle)
{
  $('.console').console({
    promptLabel: '> ',
    commandValudate: function (x) { return x!='' },
    commandHandle: function (x) { console.log('js',x); return A(handle, [[0,x], 0]) },
    autofocus:true,
    promptHistory:true
  });
}




