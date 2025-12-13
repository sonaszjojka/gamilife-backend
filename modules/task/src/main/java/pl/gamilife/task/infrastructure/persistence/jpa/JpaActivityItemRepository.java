package pl.gamilife.task.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.task.domain.model.projection.ActivityItem;

import java.util.UUID;

public interface JpaActivityItemRepository extends JpaRepository<ActivityItem, UUID>, JpaSpecificationExecutor<ActivityItem> {
}
