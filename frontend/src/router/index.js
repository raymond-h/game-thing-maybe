import VueRouter from 'vue-router';

import MainPage from '../components/MainPage';
import LoginCallback from '../components/LoginCallback';

const routes = [
  { path: '/', component: MainPage },
  { path: '/login_callback', component: LoginCallback },
];

const router = new VueRouter({
  mode: 'history',
  routes
});

export default router;
