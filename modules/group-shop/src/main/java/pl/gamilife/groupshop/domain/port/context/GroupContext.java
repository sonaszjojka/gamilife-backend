package pl.gamilife.groupshop.domain.port.context;

import pl.gamilife.groupshop.domain.model.projection.GroupForShop;

import java.util.UUID;

public interface GroupContext {
    GroupForShop findGroupById(UUID groupId);
}
