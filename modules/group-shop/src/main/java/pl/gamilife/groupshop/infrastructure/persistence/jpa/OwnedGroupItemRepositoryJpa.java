package pl.gamilife.groupshop.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.groupshop.domain.model.OwnedGroupItem;

import java.util.UUID;

public interface OwnedGroupItemRepositoryJpa extends JpaRepository<OwnedGroupItem, UUID> {
}
