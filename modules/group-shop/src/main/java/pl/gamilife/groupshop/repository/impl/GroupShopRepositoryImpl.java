package pl.gamilife.groupshop.repository.impl;

import pl.gamilife.groupshop.entity.GroupShop;
import pl.gamilife.groupshop.repository.GroupShopRepository;
import pl.gamilife.groupshop.repository.jpa.GroupShopRepositoryJpa;
import org.springframework.stereotype.Repository;

import java.util.Optional;
import java.util.UUID;

@Repository
public class GroupShopRepositoryImpl implements GroupShopRepository {
    private final GroupShopRepositoryJpa groupShopRepositoryJpa;

    public GroupShopRepositoryImpl(GroupShopRepositoryJpa groupShopRepositoryJpa) {
        this.groupShopRepositoryJpa = groupShopRepositoryJpa;
    }

    @Override
    public Optional<GroupShop> findByGroupId(UUID groupId) {
        return groupShopRepositoryJpa.findByGroupId(groupId);
    }

    @Override
    public Optional<GroupShop> findByGroupShopId(UUID groupShopId) {
        return groupShopRepositoryJpa.findById(groupShopId);
    }

    @Override
    public GroupShop save(GroupShop groupShop) {
        return groupShopRepositoryJpa.save(groupShop);
    }

    @Override
    public void deleteById(UUID groupShopId) {

        groupShopRepositoryJpa.deleteById(groupShopId);

    }
}
