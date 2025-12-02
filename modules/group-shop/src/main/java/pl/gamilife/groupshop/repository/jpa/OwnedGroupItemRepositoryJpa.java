package pl.gamilife.groupshop.repository.jpa;

import pl.gamilife.groupshop.entity.OwnedGroupItem;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface OwnedGroupItemRepositoryJpa extends JpaRepository<OwnedGroupItem, UUID> {
}
