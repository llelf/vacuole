
var Paper = Snap('#snap');

// var c1 = Paper.circle(100,100,10);
// var c2 = Paper.circle(100,200,10);

// var l = Paper.line(100,100,100,200);

function canvasClear()
{
  Snap.selectAll('#snap > :not(defs)').forEach(function(e) { e.remove() });
}

// var graph = { nodes: [{name:'12'}, {name:'b',size:23,color:'red'}, {name:'c'}, {name:'ы'}],
// 	      links: [{from:0,to:1},{from:1,to:2},{from:2,to:3}] };


var width=500;
var height=500;
var Force = d3.layout.force().size([width,height]);
Force.linkStrength(function(l) {return l.strength || 0.5});
Force.charge(-700);

function drawGraph (graph)
{
  var linksG = Paper.g();
  var nodesG = Paper.g().attr({'font-family':'verdana'});


  //Paper.circle(200,200,100).attr({fill:'url(#pat)'});
  //Paper.rect(0,0,width,height).attr({fill:'url(#pat)'});

  //Paper.circle(200,200,100);


  var nodes = [];
  for (var i = 0; i < graph.nodes.length; i++)
  {
    var n = graph.nodes[i];
    nodes[i] = Paper.g();
    var c = Paper.circle(0,0,n.size||17).attr({'class':'c'});
    var t = Paper.text(0,0,n.name).attr({'text-anchor':'middle', 'alignment-baseline':'middle'});
    nodes[i].append(c).append(t);
    nodes[i].name = n.name;
    //  nodes[i].drag();

    nodesG.append(nodes[i]);
  }

  var arrow = Paper.path('M0,-5 L15,0 L0,5')
    .attr({stroke:'red', fill:'green',transform:'scale(0.5)'})
    .marker(0,-5,15,10,15,0);

  var links = [];
  for (var i = 0; i < graph.links.length; i++)
  {
    links[i] = Paper.line(0,0,10,10).attr({'marker-end':arrow});
    var l = graph.links[i];
    links[i].source = nodes[l.from], links[i].target = nodes[l.to];
    if (links[i].target.name == '[]')
      links[i].strength = 0.1;
    //  links[i].drag();

    linksG.append(links[i]);
  }

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
}


function newInput ()
{
  var txt = d3.select('textarea').property('value');
  console.log(txt);

  d3.json('/vac')
    .post(txt,
	  function (a,b,c) {
	    console.log('ajax', a,b,c);
	    canvasClear();
	    drawGraph(b);
	  });
}

newInput();
