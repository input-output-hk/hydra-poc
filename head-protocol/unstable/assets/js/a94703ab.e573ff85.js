"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[9048],{5745:(e,t,n)=>{n.r(t),n.d(t,{default:()=>be});var a=n(96540),o=n(18215),i=n(45500),s=n(17559),l=n(26972),r=n(60609),c=n(21312),d=n(23104),u=n(75062);const m={backToTopButton:"backToTopButton_sjWU",backToTopButtonShow:"backToTopButtonShow_xfvO"};var h=n(74848);function p(){const{shown:e,scrollToTop:t}=function(e){let{threshold:t}=e;const[n,o]=(0,a.useState)(!1),i=(0,a.useRef)(!1),{startScroll:s,cancelScroll:l}=(0,d.gk)();return(0,d.Mq)(((e,n)=>{let{scrollY:a}=e;const s=n?.scrollY;s&&(i.current?i.current=!1:a>=s?(l(),o(!1)):a<t?o(!1):a+window.innerHeight<document.documentElement.scrollHeight&&o(!0))})),(0,u.$)((e=>{e.location.hash&&(i.current=!0,o(!1))})),{shown:n,scrollToTop:()=>s(0)}}({threshold:300});return(0,h.jsx)("button",{"aria-label":(0,c.T)({id:"theme.BackToTopButton.buttonAriaLabel",message:"Scroll back to top",description:"The ARIA label for the back to top button"}),className:(0,o.A)("clean-btn",s.G.common.backToTopButton,m.backToTopButton,e&&m.backToTopButtonShow),type:"button",onClick:t})}var b=n(53109),x=n(56347),f=n(24581),C=n(20053),j=n(6342),_=n(75033);const v=e=>(0,h.jsx)("svg",{xmlns:"http://www.w3.org/2000/svg",width:"24",height:"24",viewBox:"0 0 24 24",...e,children:(0,h.jsx)("path",{d:"M15.3258 14.9258V9.07578L12.3758 12.0008L15.3258 14.9258ZM5.80078 19.7008C5.38411 19.7008 5.02995 19.5549 4.73828 19.2633C4.44661 18.9716 4.30078 18.6174 4.30078 18.2008V5.80078C4.30078 5.38411 4.44661 5.02995 4.73828 4.73828C5.02995 4.44661 5.38411 4.30078 5.80078 4.30078H18.2008C18.6174 4.30078 18.9716 4.44661 19.2633 4.73828C19.5549 5.02995 19.7008 5.38411 19.7008 5.80078V18.2008C19.7008 18.6174 19.5549 18.9716 19.2633 19.2633C18.9716 19.5549 18.6174 19.7008 18.2008 19.7008H5.80078ZM8.00078 19.0008V5.00078H5.80078C5.60078 5.00078 5.41745 5.08411 5.25078 5.25078C5.08411 5.41745 5.00078 5.60078 5.00078 5.80078V18.2008C5.00078 18.4008 5.08411 18.5841 5.25078 18.7508C5.41745 18.9174 5.60078 19.0008 5.80078 19.0008H8.00078ZM8.70078 19.0008H18.2008C18.4008 19.0008 18.5841 18.9174 18.7508 18.7508C18.9174 18.5841 19.0008 18.4008 19.0008 18.2008V5.80078C19.0008 5.60078 18.9174 5.41745 18.7508 5.25078C18.5841 5.08411 18.4008 5.00078 18.2008 5.00078H8.70078V19.0008Z",fill:"black"})});function g(e){let{onClick:t}=e;return(0,h.jsxs)("button",{type:"button",title:(0,c.T)({id:"theme.docs.sidebar.collapseButtonTitle",message:"Collapse sidebar",description:"The title attribute for collapse button of doc sidebar"}),"aria-label":(0,c.T)({id:"theme.docs.sidebar.collapseButtonAriaLabel",message:"Collapse sidebar",description:"The title attribute for collapse button of doc sidebar"}),className:(0,C.A)("inline-flex p-4 mx-6 justify-between"),onClick:t,children:["Collapse side panel ",(0,h.jsx)(v,{})]})}var A=n(65041),k=n(89532);const S=Symbol("EmptyContext"),T=a.createContext(S);function N(e){let{children:t}=e;const[n,o]=(0,a.useState)(null),i=(0,a.useMemo)((()=>({expandedItem:n,setExpandedItem:o})),[n]);return(0,h.jsx)(T.Provider,{value:i,children:t})}var I=n(41422),w=n(99169),y=n(28774),B=n(92303);function L(e){let{collapsed:t,categoryLabel:n,onClick:a}=e;return(0,h.jsx)("button",{"aria-label":t?(0,c.T)({id:"theme.DocSidebarItem.expandCategoryAriaLabel",message:"Expand sidebar category '{label}'",description:"The ARIA label to expand the sidebar category"},{label:n}):(0,c.T)({id:"theme.DocSidebarItem.collapseCategoryAriaLabel",message:"Collapse sidebar category '{label}'",description:"The ARIA label to collapse the sidebar category"},{label:n}),"aria-expanded":!t,type:"button",className:"clean-btn menu__caret",onClick:a})}function H(e){let{item:t,onItemClick:n,activePath:i,level:r,index:c,...d}=e;const{items:u,label:m,collapsible:p,className:b,href:x}=t,{docs:{sidebar:{autoCollapseCategories:f}}}=(0,j.p)(),C=function(e){const t=(0,B.A)();return(0,a.useMemo)((()=>e.href&&!e.linkUnlisted?e.href:!t&&e.collapsible?(0,l.Nr)(e):void 0),[e,t])}(t),_=(0,l.w8)(t,i),v=(0,w.ys)(x,i),{collapsed:g,setCollapsed:A}=(0,I.u)({initialState:()=>!!p&&(!_&&t.collapsed)}),{expandedItem:N,setExpandedItem:H}=function(){const e=(0,a.useContext)(T);if(e===S)throw new k.dV("DocSidebarItemsExpandedStateProvider");return e}(),M=function(e){void 0===e&&(e=!g),H(e?null:c),A(e)};return function(e){let{isActive:t,collapsed:n,updateCollapsed:o}=e;const i=(0,k.ZC)(t);(0,a.useEffect)((()=>{t&&!i&&n&&o(!1)}),[t,i,n,o])}({isActive:_,collapsed:g,updateCollapsed:M}),(0,a.useEffect)((()=>{p&&null!=N&&N!==c&&f&&A(!0)}),[p,N,c,A,f]),(0,h.jsxs)("li",{className:(0,o.A)(s.G.docs.docSidebarItemCategory,s.G.docs.docSidebarItemCategoryLevel(r),"menu__list-item",{"menu__list-item--collapsed":g},b),children:[(0,h.jsxs)("div",{className:(0,o.A)("menu__list-item-collapsible",{"menu__list-item-collapsible--active":v}),children:[(0,h.jsx)(y.A,{className:(0,o.A)("menu__link",{"menu__link--sublist":p,"menu__link--sublist-caret":!x&&p,"menu__link--active":_}),onClick:p?e=>{n?.(t),x?M(!1):(e.preventDefault(),M())}:()=>{n?.(t)},"aria-current":v?"page":void 0,role:p&&!x?"button":void 0,"aria-expanded":p&&!x?!g:void 0,href:p?C??"#":C,...d,children:m}),x&&p&&(0,h.jsx)(L,{collapsed:g,categoryLabel:m,onClick:e=>{e.preventDefault(),M()}})]}),(0,h.jsx)(I.N,{lazy:!0,as:"ul",className:"menu__list",collapsed:g,children:(0,h.jsx)(U,{items:u,tabIndex:g?-1:0,onItemClick:n,activePath:i,level:r+1})})]})}var M=n(16654),E=n(1989);const P="menuExternalLink_NmtK";function R(e){let{item:t,onItemClick:n,activePath:a,level:i,index:r,...c}=e;const{href:d,label:u,className:m,autoAddBaseUrl:p}=t,b=(0,l.w8)(t,a),x=(0,M.A)(d);return(0,h.jsx)("li",{className:(0,o.A)(s.G.docs.docSidebarItemLink,s.G.docs.docSidebarItemLinkLevel(i),"menu__list-item",m),children:(0,h.jsxs)(y.A,{className:(0,o.A)("menu__link",!x&&P,{"menu__link--active":b}),autoAddBaseUrl:p,"aria-current":b?"page":void 0,to:d,...x&&{onClick:n?()=>n(t):void 0},...c,children:[u,!x&&(0,h.jsx)(E.A,{})]})},u)}const G="menuHtmlItem_M9Kj";function V(e){let{item:t,level:n,index:a}=e;const{value:i,defaultStyle:l,className:r}=t;return(0,h.jsx)("li",{className:(0,o.A)(s.G.docs.docSidebarItemLink,s.G.docs.docSidebarItemLinkLevel(n),l&&[G,"menu__list-item"],r),dangerouslySetInnerHTML:{__html:i}},a)}function W(e){let{item:t,...n}=e;switch(t.type){case"category":return(0,h.jsx)(H,{item:t,...n});case"html":return(0,h.jsx)(V,{item:t,...n});default:return(0,h.jsx)(R,{item:t,...n})}}function D(e){let{items:t,...n}=e;const a=(0,l.Y)(t,n.activePath);return(0,h.jsx)(N,{children:a.map(((e,t)=>(0,h.jsx)(W,{item:e,index:t,...n},t)))})}const U=(0,a.memo)(D),F="menu_Y1UP",Y="menuWithAnnouncementBar_fPny";function Z(e){let{path:t,sidebar:n,className:o}=e;const i=function(){const{isActive:e}=(0,A.M)(),[t,n]=(0,a.useState)(e);return(0,d.Mq)((t=>{let{scrollY:a}=t;e&&n(0===a)}),[e]),e&&t}();return(0,h.jsx)("nav",{"aria-label":(0,c.T)({id:"theme.docs.sidebar.navAriaLabel",message:"Docs sidebar",description:"The ARIA label for the sidebar navigation"}),className:(0,C.A)("menu thin-scrollbar",F,i&&Y,o),children:(0,h.jsx)("ul",{className:(0,C.A)(s.G.docs.docSidebarMenu,"menu__list"),children:(0,h.jsx)(U,{items:n,activePath:t,level:1})})})}const z="sidebar_mhZE",K="sidebarWithHideableNavbar__6UL",O="sidebarHidden__LRd",q="sidebarLogo_F_0z";function J(e){let{path:t,sidebar:n,onCollapse:a,isHidden:o}=e;const{navbar:{hideOnScroll:i},docs:{sidebar:{hideable:s}}}=(0,j.p)();return(0,h.jsxs)("div",{className:(0,C.A)(z,i&&K,o&&O),children:[i&&(0,h.jsx)(_.A,{tabIndex:-1,className:q}),(0,h.jsx)(Z,{path:t,sidebar:n}),(0,h.jsx)(g,{onClick:a})]})}const Q=a.memo(J);var X=n(75600),$=n(22069);const ee=e=>{let{sidebar:t,path:n}=e;const a=(0,$.M)();return(0,h.jsx)("ul",{className:(0,o.A)(s.G.docs.docSidebarMenu,"menu__list"),children:(0,h.jsx)(U,{items:t,activePath:n,onItemClick:e=>{"category"===e.type&&e.href&&a.toggle(),"link"===e.type&&a.toggle()},level:1})})};function te(e){return(0,h.jsx)(X.GX,{component:ee,props:e})}const ne=a.memo(te);function ae(e){const t=(0,f.l)(),n="desktop"===t||"ssr"===t,a="mobile"===t;return(0,h.jsxs)(h.Fragment,{children:[n&&(0,h.jsx)(Q,{...e}),a&&(0,h.jsx)(ne,{...e})]})}function oe(e){return(0,h.jsx)("svg",{width:"20",height:"20","aria-hidden":"true",...e,children:(0,h.jsxs)("g",{fill:"#7a7a7a",children:[(0,h.jsx)("path",{d:"M9.992 10.023c0 .2-.062.399-.172.547l-4.996 7.492a.982.982 0 01-.828.454H1c-.55 0-1-.453-1-1 0-.2.059-.403.168-.551l4.629-6.942L.168 3.078A.939.939 0 010 2.528c0-.548.45-.997 1-.997h2.996c.352 0 .649.18.828.45L9.82 9.472c.11.148.172.347.172.55zm0 0"}),(0,h.jsx)("path",{d:"M19.98 10.023c0 .2-.058.399-.168.547l-4.996 7.492a.987.987 0 01-.828.454h-3c-.547 0-.996-.453-.996-1 0-.2.059-.403.168-.551l4.625-6.942-4.625-6.945a.939.939 0 01-.168-.55 1 1 0 01.996-.997h3c.348 0 .649.18.828.45l4.996 7.492c.11.148.168.347.168.55zm0 0"})]})})}const ie={expandButton:"expandButton_TmdG",expandButtonIcon:"expandButtonIcon_i1dp"};function se(e){let{toggleSidebar:t}=e;return(0,h.jsx)("div",{className:ie.expandButton,title:(0,c.T)({id:"theme.docs.sidebar.expandButtonTitle",message:"Expand sidebar",description:"The ARIA label and title attribute for expand button of doc sidebar"}),"aria-label":(0,c.T)({id:"theme.docs.sidebar.expandButtonAriaLabel",message:"Expand sidebar",description:"The ARIA label and title attribute for expand button of doc sidebar"}),tabIndex:0,role:"button",onKeyDown:t,onClick:t,children:(0,h.jsx)(oe,{className:ie.expandButtonIcon})})}const le={docSidebarContainer:"docSidebarContainer_YfHR",docSidebarContainerHidden:"docSidebarContainerHidden_DPk8",sidebarViewport:"sidebarViewport_aRkj"};function re(e){let{children:t}=e;const n=(0,r.t)();return(0,h.jsx)(a.Fragment,{children:t},n?.name??"noSidebar")}function ce(e){let{sidebar:t,hiddenSidebarContainer:n,setHiddenSidebarContainer:i}=e;const{pathname:l}=(0,x.zy)(),[r,c]=(0,a.useState)(!1),d=(0,a.useCallback)((()=>{r&&c(!1),!r&&(0,b.O)()&&c(!0),i((e=>!e))}),[i,r]);return(0,h.jsx)("aside",{className:(0,o.A)(s.G.docs.docSidebarContainer,le.docSidebarContainer,n&&le.docSidebarContainerHidden),onTransitionEnd:e=>{e.currentTarget.classList.contains(le.docSidebarContainer)&&n&&c(!0)},children:(0,h.jsx)(re,{children:(0,h.jsxs)("div",{className:(0,o.A)(le.sidebarViewport,r&&le.sidebarViewportHidden),children:[(0,h.jsx)(ae,{sidebar:t,path:l,onCollapse:d,isHidden:r}),r&&(0,h.jsx)(se,{toggleSidebar:d})]})})})}const de={docMainContainer:"docMainContainer_TBSr",docMainContainerEnhanced:"docMainContainerEnhanced_lQrH",docItemWrapperEnhanced:"docItemWrapperEnhanced_JWYK"};function ue(e){let{hiddenSidebarContainer:t,children:n}=e;const a=(0,r.t)();return(0,h.jsx)("main",{className:(0,o.A)(de.docMainContainer,(t||!a)&&de.docMainContainerEnhanced),children:(0,h.jsx)("div",{className:(0,o.A)("container padding-top--md padding-bottom--lg",de.docItemWrapper,t&&de.docItemWrapperEnhanced),children:n})})}const me={docRoot:"docRoot_UBD9",docsWrapper:"docsWrapper_hBAB"};function he(e){let{children:t}=e;const n=(0,r.t)(),[o,i]=(0,a.useState)(!1);return(0,h.jsxs)("div",{className:me.docsWrapper,children:[(0,h.jsx)(p,{}),(0,h.jsxs)("div",{className:me.docRoot,children:[n&&(0,h.jsx)(ce,{sidebar:n.items,hiddenSidebarContainer:o,setHiddenSidebarContainer:i}),(0,h.jsx)(ue,{hiddenSidebarContainer:o,children:t})]})]})}var pe=n(23363);function be(e){const t=(0,l.B5)(e);if(!t)return(0,h.jsx)(pe.A,{});const{docElement:n,sidebarName:a,sidebarItems:c}=t;return(0,h.jsx)(i.e3,{className:(0,o.A)(s.G.page.docsDocPage),children:(0,h.jsx)(r.V,{name:a,items:c,children:(0,h.jsx)(he,{children:n})})})}},23363:(e,t,n)=>{n.d(t,{A:()=>l});n(96540);var a=n(18215),o=n(21312),i=n(6028),s=n(74848);function l(e){let{className:t}=e;return(0,s.jsx)("main",{className:(0,a.A)("container margin-vert--xl",t),children:(0,s.jsx)("div",{className:"row",children:(0,s.jsxs)("div",{className:"col col--6 col--offset-3",children:[(0,s.jsx)(i.A,{as:"h1",className:"hero__title",children:(0,s.jsx)(o.A,{id:"theme.NotFound.title",description:"The title of the 404 page",children:"Page Not Found"})}),(0,s.jsx)("p",{children:(0,s.jsx)(o.A,{id:"theme.NotFound.p1",description:"The first paragraph of the 404 page",children:"We could not find what you were looking for."})}),(0,s.jsx)("p",{children:(0,s.jsx)(o.A,{id:"theme.NotFound.p2",description:"The 2nd paragraph of the 404 page",children:"Please contact the owner of the site that linked you to the original URL and let them know their link is broken."})})]})})})}}}]);