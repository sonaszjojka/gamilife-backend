package edu.pjwstk.groupshop.repository;

import edu.pjwstk.groupshop.entity.GroupShop;

import java.util.Optional;
import java.util.UUID;

public interface GroupShopRepository {
    Optional<GroupShop> findByGroupId(UUID id);
    GroupShop save(GroupShop groupShop);

    void deleteById(UUID groupShopId);

    Optional<GroupShop> findByGroupShopId(UUID groupShopId);
}
