import Vue from 'vue';
import VueRouter from 'vue-router';
import VueRx from 'vue-rx';

import App from './components/App';
import router from './router';

Vue.use(VueRouter);
Vue.use(VueRx);

new Vue({
  el: '#root',
  render: h => h(App),
  router
});
