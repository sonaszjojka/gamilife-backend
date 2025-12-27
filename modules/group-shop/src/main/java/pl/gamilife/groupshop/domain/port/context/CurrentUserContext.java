package pl.gamilife.groupshop.domain.port.context;

import pl.gamilife.groupshop.domain.model.projection.GroupShopUser;

import java.util.UUID;

public interface CurrentUserContext {

    GroupShopUser findGroupShopUserById(UUID userId);
}
