package pl.gamilife.groupshop.infrastructure.persistence;

import org.springframework.stereotype.Repository;
import pl.gamilife.groupshop.domain.model.GroupShop;
import pl.gamilife.groupshop.domain.port.repository.GroupShopRepository;
import pl.gamilife.groupshop.infrastructure.persistence.jpa.GroupShopRepositoryJpa;

import java.util.Optional;
import java.util.UUID;

@Repository
public class GroupShopRepositoryAdapter implements GroupShopRepository {
    private final GroupShopRepositoryJpa groupShopRepositoryJpa;

    public GroupShopRepositoryAdapter(GroupShopRepositoryJpa groupShopRepositoryJpa) {
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
    public void deleteByGroupId(UUID groupId) {
        groupShopRepositoryJpa.deleteByGroupId(groupId);
    }

    @Override
    public GroupShop save(GroupShop groupShop) {
        return groupShopRepositoryJpa.save(groupShop);
    }
}
