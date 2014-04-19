///*
// *  Copyright 2012 by The Cogchar Project (www.cogchar.org).
// * 
// *  Licensed under the Apache License, Version 2.0 (the "License");
// *  you may not use this file except in compliance with the License.
// *  You may obtain a copy of the License at
// * 
// *       http://www.apache.org/licenses/LICENSE-2.0
// * 
// *  Unless required by applicable law or agreed to in writing, software
// *  distributed under the License is distributed on an "AS IS" BASIS,
// *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// *  See the License for the specific language governing permissions and
// *  limitations under the License.
// */
package org.cogchar.app.puma.vworld;;
//
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//import java.util.List;
//import org.appdapter.core.log.BasicDebugger;
//import org.appdapter.core.name.Ident;
//import org.appdapter.help.repo.RepoClient;
//import org.cogchar.impl.thing.basic.BasicThingActionConsumer;
//import org.cogchar.impl.thing.basic.BasicThingActionRouter;
//import org.cogchar.app.puma.boot.PumaAppContext;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.ALL_HUMANOID_CONFIG;
//import static org.cogchar.app.puma.boot.PumaContextCommandBox.WORLD_CONFIG;
//import org.cogchar.app.puma.config.PumaConfigManager;
//import org.cogchar.app.puma.config.PumaGlobalModeManager;
//import org.cogchar.name.entity.EntityRoleCN;
//import org.cogchar.blob.emit.*;
//import org.cogchar.platform.gui.keybind.KeyBindingConfig;
//import org.cogchar.platform.trigger.CommandSpace;
//import org.cogchar.render.app.humanoid.HumanoidRenderContext;
//import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper;
//import org.cogchar.render.goody.basic.DataballGoodyBuilder;
//import org.cogchar.render.app.entity.GoodyFactory;
//import org.cogchar.render.app.entity.VWorldEntityActionConsumer;
//import org.cogchar.render.opengl.osgi.RenderBundleUtils;
//import org.cogchar.render.sys.context.CogcharRenderContext;
//import org.cogchar.render.sys.input.VW_HelpScreenMgr;
//import org.cogchar.render.sys.input.VW_InputBindingFuncs;
//import org.cogchar.render.sys.module.RenderGateway;
//import org.cogchar.render.sys.module.RenderModule;
//import org.cogchar.render.sys.registry.RenderRegistryClient;
//import org.cogchar.render.sys.goody.GoodyRenderRegistryClient;
//import org.osgi.framework.BundleContext;
//
///**
// * @author Stu B. <www.texpedient.com>
// */
public class PumaVirtualWorldMapper /*extends BasicDebugger implements RenderGateway */{
//
//	private CogcharRenderContext myCRC;
//	private PumaAppContext myPAC;
//	
//	private	VWorldMonitorBinding	myVWMonitorBinding;
//
//	public PumaVirtualWorldMapper(PumaAppContext pac) {
//		myPAC = pac;
//	}
//
//	public HumanoidRenderContext getHumanoidRenderContext() {
//		return (HumanoidRenderContext) myCRC;
//	}
//	@Override public CogcharRenderContext getCogcharRenderContext() {
//		return myCRC;
//	}
//	/**
//	 * First (of three) stage init of world, done BEFORE startOpenGLCanvas().
//	 *
//	 * @param panelKind
//	 * @return
//	 */
//	public HumanoidRenderContext initHumanoidRenderContext(String panelKind) {
//		BundleContext bundleCtx = myPAC.getBundleContext();
//		myCRC = (HumanoidRenderContext) RenderBundleUtils.buildBonyRenderContextInOSGi(bundleCtx, panelKind);
//
//		return (HumanoidRenderContext)  myCRC;
//	}
//
//// The Lights/Camera/Cinematics init used to be done from HumanoidRenderContext, but the global config lives
//	// here as does humanoid and bony config. So may make sense to have this here too, though we could move it
//	// back to HRC if there are philosophical reasons for doing so. (We'd also have to pass two graph flavors to it for this.)
//	// Added: since jMonkey key bindings are part of "virtual world" config like Lights/Camera/Cinematics, they are also 
//	// set here
//	public void initVirtualWorlds(CommandSpace cspace, PumaConfigManager pcm, BasicThingActionRouter router) { 
//// , PumaWebMapper webMapper, 	BundleContext bundleCtx) {
//		final PumaGlobalModeManager pgmm = pcm.getGlobalModeMgr();
//		GlobalConfigEmitter gce = pgmm.getGlobalConfig();
//		
//		RepoClient rc = pcm.getMainConfigRepoClient();		
//		HumanoidRenderContext hrc = getHumanoidRenderContext();
//		hrc.initCinematicParameters();
//		hrc.setupHominoidCameraManager();
//		
//		KeyBindingConfig currKeyBindCfg = new KeyBindingConfig();
//		// Hook-in for Goody system
//		GoodyRenderRegistryClient grrc = hrc.getGoodyRenderRegistryClient();
//		GoodyFactory gFactory = GoodyFactory.createTheFactory(grrc, hrc);
//		// Setup the humanoid "goodies"
//		HumanoidRenderWorldMapper hrwMapper = new HumanoidRenderWorldMapper();
//		VWorldEntityActionConsumer veActConsumer = gFactory.getActionConsumer();
//		hrwMapper.addHumanoidGoodies(veActConsumer, hrc);
//		try {
//			List<Ident> worldConfigIdents = gce.entityMap().get(EntityRoleCN.VIRTUAL_WORLD_ENTITY_TYPE);
//			// Multiple worldConfigIdents? Possible. It's possible duplicate cinematic definitions might cause problems
//			// but we'll leave that for later, so sure, go ahead and load on multiple configs if they are requested.
//			for (Ident configIdent : worldConfigIdents) {
//				// This may try to add a head-cam, so we need to have injected the HominoidCameraManager already
//				// (done 10 lines above).
//				initCinematicStuff(gce, configIdent, rc, gFactory, router);
//				// Like with everything else dependent on global config's graph settings (except for Lift, which uses a managed service
//				// version of GlobalConfigEmitter) it seems logical to set the key bindings here.
//				// Multiple worldConfigIdents? We decided above this is possible (if messy). If key bindings are duplicated
//				// between the multiple world configs, we can't be certain which will end up in the KeyBindingConfig map.
//				// But for now we'll assume user is smart enough to watch out for that (perhaps a dangerous idea) and pile
//				// bindings from all worldConfigIdents into our KeyBindingConfig instance.
//				try {
//					Ident graphIdent = gce.ergMap().get(configIdent).get(EntityRoleCN.INPUT_BINDINGS_ROLE);
//					KeystrokeConfigEmitter kce = new KeystrokeConfigEmitter();
//
//					currKeyBindCfg.addBindings(rc, graphIdent, kce);
//				} catch (Exception e) {
//					getLogger().error("Could not get valid graph on which to query for input bindings config of {}",
//							configIdent.getLocalName(), e);
//				}
//			}
//		} catch (Exception e) {
//			getLogger().error("Could not retrieve any specified VirtualWorldEntity for this global configuration!");
//		}
//		
//		hrc.refreshInputBindingsAndHelpScreen(currKeyBindCfg, cspace);
//	}
//	/**
//	 * 
//	 * @param gce
//	 * @param worldConfigIdent
//	 * @param repoCli
//	 * @param gFactory
//	 * @param router 
//	 * 
//	 * This method embodies many of our brittle old assumption-scaffolding used to get our 
//	 * configurable V-world up and running.
//	 */
//	private void initCinematicStuff(GlobalConfigEmitter gce, Ident worldConfigIdent, RepoClient repoCli, 
//			GoodyFactory gFactory, BasicThingActionRouter router) {
//		HumanoidRenderWorldMapper renderMapper = new HumanoidRenderWorldMapper();
//		HumanoidRenderContext hrc = getHumanoidRenderContext();
//		Ident graphIdent = null;
//		try {
//			graphIdent = gce.ergMap().get(worldConfigIdent).get(EntityRoleCN.LIGHTS_CAMERA_CONFIG_ROLE);
//		} catch (Exception e) {
//			getLogger().warn("Could not get valid graph on which to query for Lights/Cameras config of {}", worldConfigIdent.getLocalName(), e);
//		}
//		try {
//			renderMapper.initLightsAndCamera(repoCli, hrc, graphIdent);
//		} catch (Exception e) {
//			getLogger().warn("Error attempting to initialize lights and cameras for {}: ", worldConfigIdent.getLocalName(), e);
//		}
//		setupActionConsumer(router, gce, worldConfigIdent, repoCli, gFactory);
//		graphIdent = null;
//		try {
//			graphIdent = gce.ergMap().get(worldConfigIdent).get(EntityRoleCN.WAYPOINTS_BINDINGS_ROLE);
//			renderMapper.initWaypoints(repoCli, graphIdent);
//		} catch (Exception e) {
//			getLogger().error("Could not initialize waypoints/orientations with a config of {}",
//					worldConfigIdent.getLocalName(), e);
//		}
//		graphIdent = null;
//		try {
//			graphIdent = gce.ergMap().get(worldConfigIdent).get(EntityRoleCN.MOTIONPATH_CONFIG_ROLE);
//		} catch (Exception e) {
//			getLogger().warn("Could not get valid graph on which to query for Paths config of {}", worldConfigIdent.getLocalName(), e);
//		}
//		try {
//			renderMapper.initPaths(repoCli, hrc, graphIdent);
//		} catch (Exception e) {
//			getLogger().warn("Error attempting to initialize Paths for {}: ", worldConfigIdent.getLocalName(), e);
//		}
//		try {
//			graphIdent = gce.ergMap().get(worldConfigIdent).get(EntityRoleCN.THING_ANIM_BINDINGS_ROLE);
//			renderMapper.initThingAnims(repoCli, hrc, graphIdent);
//		} catch (Exception e) {
//			getLogger().error("Could not initialize Thing spatial animations with a config of {}",
//					worldConfigIdent.getLocalName(), e);
//		}
//	}
//	/**
//	 * 
//	 * @param router
//	 * @param gce
//	 * @param worldConfigID
//	 * @param repoCli
//	 * @param gFactory 
//	 * 
//	 * Let's review and figure out what is really going on here.
//	 * 
//	 * The stack at this point [as of 2013-10-06] is:
//	 *	initCinematicStuff
//	 *	initVirtualWorlds
//	 *	PumaAppContext.initCinema - *  (see Javadoc comment above this method).
//	 *		- called from 3 different places, including pumaBoot and processUpdateRequestNow()
//	 */
//	public void setupActionConsumer(BasicThingActionRouter router, GlobalConfigEmitter gce, Ident worldConfigID, 
//				RepoClient repoCli, GoodyFactory gFactory){
//		// Goodies should be initialized before paths/animations so that they can reference Goodies!
//		try {
//			Ident actionGraphID = gce.ergMap().get(worldConfigID).get(EntityRoleCN.THING_ACTIONS_BINDINGS_ROLE);
//			BasicThingActionConsumer consumer = 	gFactory.getActionConsumer();
//			// We consume the actions here because...(?) [Something-something clear the "old"[/"init"] actions]
//			// Note here is the *only* use of this deprecated 2-args method form (in all of Cogchar), so refactoring 
//			// it out of this call in Puma will allow us to remove it from o.c.lib.core.
//			consumer.consumeAllActions(repoCli, actionGraphID);
//			router.appendConsumer(actionGraphID, consumer);
//			getLogger().info("Finished consumingActions and appending consumer, now attaching AppMonitor binding");
//			if (myVWMonitorBinding == null) {
//				myVWMonitorBinding = new VWorldMonitorBinding();
//			}
//			router.setAppMonitor(myVWMonitorBinding);
//			
//		} catch (Exception e) {
//			getLogger().error("Could not initialize Thing actions with a config of {}",
//					worldConfigID.getLocalName(), e);
//		}		
//	}
//	/**
//	 * Second (and most crucial) stage of OpenGL init. This method blocks until the canvas initialization is complete,
//	 * which requires that the simpleInitApp() methods have all completed.
//	 *
//	 * @param wrapInJFrameFlag
//	 * @throws Exception
//	 */
//	public void startOpenGLCanvas(boolean wrapInJFrameFlag, java.awt.event.WindowListener optWinLis) throws Exception {
//		HumanoidRenderContext hrc = getHumanoidRenderContext();
//		if (hrc != null) {
//			hrc.startOpenGLCanvas(wrapInJFrameFlag, optWinLis);
//		} else {
//			logError("HumanoidRenderContext is NULL, cannot startOpenGLCanvas!");
//		}
//	}
//
//	/**
//	 * Called from PumaAppContext.initCinema  (if clearFirst arg-flag is true)
//	 * and PumaAppContext.disconnectAllCharsAndMappers
//	 */
//	public void clearCinematicStuff() {
//		HumanoidRenderWorldMapper myRenderMapper = new HumanoidRenderWorldMapper();
//		HumanoidRenderContext hrc = getHumanoidRenderContext();
//		myRenderMapper.clearLights(hrc);
//		myRenderMapper.clearCinematics(hrc);
//		myRenderMapper.clearViewPorts(hrc);
//	}
///**
// * Called from PumaAppContext.stopAndReleaseAllHumanoids, which is called only from  
// * PumaContextCommandBox.processUpdateRequestNow
// */
//	public void detachAllHumanoidFigures() {
//		HumanoidRenderContext hrc = getHumanoidRenderContext();
//		hrc.getHumanoidFigureManager().detachHumanoidFigures(hrc);
//	}
//	/**
//	 * Called from PumaAppContext.initCinema
//	 * @param bonyRdfCl 
//	 */
//	public void connectVisualizationResources(ClassLoader bonyRdfCl) {
//		HumanoidRenderContext hrc = getHumanoidRenderContext();
//		DataballGoodyBuilder ballBldr = DataballGoodyBuilder.getTheBallBuilder();
//		ballBldr.setClassLoader("Cog Char", bonyRdfCl);
//		ballBldr.initialize(hrc);
//		hrc.setTheBallBuilder(ballBldr);
//	}	
//	// Previous functions now mostly done from within LifterLifecycle on create(). 
//	// Retaining for now for legacy BallBuilder classloader hookup
//	public void connectHrkindVisualizationContent(ClassLoader hrkindResourceCL) {
//		DataballGoodyBuilder.getTheBallBuilder().setClassLoader("hrkind.content.preview", hrkindResourceCL); // Adds this classloader to the ones Databalls know about
//	}
//	/**
//	 * Called from TriggerItems.ToggleHelp.fireOnPCCB
//	 */
//	public void toggleHelpScreenDisplay() { 
//		VW_HelpScreenMgr hsm = VW_InputBindingFuncs.getHelpScreenMgr();
//		RenderRegistryClient rrc = myCRC.getRenderRegistryClient();
//		hsm.toggleHelpTextDisplay(rrc);
//	}
//	/**
//	 * Called from PumaAppUtils.attachVWorldRenderModule, which is typically called from a FrameworkStartedEvent.
//	 * @param rModule 
//	 */
//	public void attachRenderModule(RenderModule rModule) {
//		HumanoidRenderContext hrc = getHumanoidRenderContext();
//		rModule.setRenderGateway(this);
//		hrc.attachModule(rModule);
//		
//	}
//	public void detachRenderModule(RenderModule rModule) {
//		HumanoidRenderContext hrc = getHumanoidRenderContext();
//		hrc.detachModule(rModule);
//		rModule.setRenderGateway(null);
//	}
}
