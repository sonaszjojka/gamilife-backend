package pl.gamilife.groupshop.repository.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.groupshop.entity.OwnedGroupItem;

import java.util.UUID;

public interface OwnedGroupItemRepositoryJpa extends JpaRepository<OwnedGroupItem, UUID> {
}
