
var Paper = Snap('#snap');

function canvasClear()
{
  console.log('clear');
  Snap.selectAll('#snap > :not(defs)').forEach(function(e) { e.remove() });
  return {};
}



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





var width=800;
var height=500;
var Force = d3.layout.force().size([width,height]);
Force.linkStrength(function(l) {return l.strength || 0.5});
Force.charge(-1000);


function hslistToJS (list)
{
  var r = [];
  for (var p = list; p[0] != 0;)
  {
    var el = p[1], tail = p[2];
    r.push(el[1]);
    p = tail;
  }

  return r;
}


function drawGraph (nodesS,linksS,fromTo,tickN,tickL)
{
  console.log('draw');

  var nodes = hslistToJS(nodesS);
  var nodesF = [];

  for (var i = 0; i < nodes.length; i++)
  {
    nodesF.push({id:i});
  }

  var links = hslistToJS(linksS);
  var linksF = [];
  for (var i = 0; i < fromTo.length; i++)
  {
    var l = {};
    l.source = nodesF[fromTo[i][0]];
    l.target = nodesF[fromTo[i][1]];
    l.id = i;
    linksF.push(l);
  }

  Force.nodes(nodesF);
  Force.links(linksF);

  Force.on('tick', function() {

    //A(tick, [0]);

    nodesF.map (function(f) {
      var n=nodesF[f.id];
      //nodes[f.id].attr({transform: 'translate('+n.x+','+n.y+')'});
      A(tickN, [toHS(n), [0,nodes[f.id]], 0]);

    });

    linksF.map (function(l) {
      var src = nodesF[l.source.id];
      var dst = nodesF[l.target.id];

      var e = links[l.id];
      // links[l.id].attr({x1:src.x, y1:src.y,
      // 			x2:dst.x, y2:dst.y})

      A(tickL, [[0,src.x], [0,src.y], [0,dst.x], [0,dst.y],
		[0,e], 0]);

    });

  });

  Force.drag();
  Force.start();

  return {};
}


