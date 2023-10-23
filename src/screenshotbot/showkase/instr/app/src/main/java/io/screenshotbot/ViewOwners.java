package io.screenshotbot;

import androidx.activity.OnBackPressedDispatcher;
import androidx.activity.OnBackPressedDispatcherOwner;
import androidx.lifecycle.Lifecycle;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.LifecycleRegistry;
import androidx.savedstate.SavedStateRegistry;
import androidx.savedstate.SavedStateRegistryController;
import androidx.savedstate.SavedStateRegistryOwner;

public class ViewOwners {
    public static class MyLifecycleOwner implements LifecycleOwner {
        private Lifecycle registry = null;

        public MyLifecycleOwner() {
            registry = new LifecycleRegistry(this);
        }

        @Override
        public Lifecycle getLifecycle() {
            return registry;
        }

    }

    public static class MySavedStateRegistryOwner implements SavedStateRegistryOwner, LifecycleOwner {
        private SavedStateRegistryController controller;
        private SavedStateRegistry registry;
        private LifecycleOwner lifecycleOwner;

        public MySavedStateRegistryOwner(LifecycleOwner _lifecycleOwner) {
            this.lifecycleOwner = _lifecycleOwner;
            controller = SavedStateRegistryController.create(this);
            controller.performRestore(null);

            registry = controller.getSavedStateRegistry();

        }

        @Override
        public SavedStateRegistry getSavedStateRegistry() {
            return registry;
        }

        @Override
        public Lifecycle getLifecycle() {
            return lifecycleOwner.getLifecycle();
        }
    }
}
