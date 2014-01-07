
var Paper = Snap('#snap');

function canvasClear()
{
  console.log('clear');
  Snap.selectAll('#snap > :not(defs)').forEach(function(e) { e.remove() });
  return {};
}


var width=500;
var height=500;
var Force = d3.layout.force().size([width,height]);
Force.linkStrength(function(l) {return l.strength || 0.5});
Force.charge(-700);


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


function drawGraph (graphS,nodesS,linksS,fromTo)
{
  var graph = JSON.parse (graphS);

  console.log('draw',fromTo);

  var linksG = Paper.g();
  var nodesG = Paper.g().attr({'font-family':'verdana'});

  var nodes = hslistToJS(nodesS);

  for (var i = 0; i < nodes.length; i++)
  {
    nodes[i].name = '?'+i;
    nodes[i].weight = 1;
    nodesG.append(nodes[i]);
  }

  console.log('nodes',nodes);
  
  var arrow = Paper.path('M0,-5 L15,0 L0,5')
    .attr({stroke:'red', fill:'green',transform:'scale(0.5)'})
    .marker(0,-5,15,10,15,0);

  var links = hslistToJS(linksS);

  for (var i = 0; i < fromTo.length; i++)
  {
    var l = links[i];
    l.source = nodes[fromTo[i][0]];
    l.target = nodes[fromTo[i][1]];
    linksG.append(l);
  }

  console.log('links',links);

  Force.nodes(nodes);
  Force.links(links);

  Force.on('tick', function() {
    //console.log(a);
    nodes.map (function(n) { n.attr({transform: 'translate('+n.x+','+n.y+')'}) });
    links.map (function(l) { l.attr({x1:l.source.x, y1:l.source.y,
				     x2:l.target.x, y2:l.target.y}) });

  });

  Force.drag();
  Force.start();

  return {};
}


