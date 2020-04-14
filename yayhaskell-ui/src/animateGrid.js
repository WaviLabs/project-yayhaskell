import { wrapGrid } from 'animate-css-grid';

const grid = document.querySelector(".grid-container");
wrapGrid( grid
        , { stagger: 0,
            // int: default is 250 ms
            duration: 500,
            // function: called with list of elements about to animate
            onStart: (animatingElementList)=> {},
            // function: called with list of elements that just finished animating
            // cancelled animations will not trigger onEnd
            onEnd: (animatingElementList)=> {}
          }
        );
