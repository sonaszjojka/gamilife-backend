package pl.gamilife.groupshop.domain.port.repository;

import pl.gamilife.groupshop.domain.model.GroupShop;

import java.util.Optional;
import java.util.UUID;

public interface GroupShopRepository {
    Optional<GroupShop> findByGroupId(UUID id);

    GroupShop save(GroupShop groupShop);

    Optional<GroupShop> findByGroupShopId(UUID groupShopId);
}
