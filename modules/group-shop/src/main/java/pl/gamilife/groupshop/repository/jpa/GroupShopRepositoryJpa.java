package pl.gamilife.groupshop.repository.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.groupshop.entity.GroupShop;

import java.util.Optional;
import java.util.UUID;

public interface GroupShopRepositoryJpa extends JpaRepository<GroupShop, UUID> {
    Optional<GroupShop> findByGroupId(UUID groupId);
}
