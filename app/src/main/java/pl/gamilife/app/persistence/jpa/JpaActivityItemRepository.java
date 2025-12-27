package pl.gamilife.app.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.app.persistence.view.ActivityItem;

import java.util.UUID;

public interface JpaActivityItemRepository extends JpaRepository<ActivityItem, UUID>, JpaSpecificationExecutor<ActivityItem> {
}