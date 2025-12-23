package pl.gamilife.task.domain.port.repository;

import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.filter.ActivityItemFilter;
import pl.gamilife.task.domain.model.projection.ActivityItem;

public interface ActivityItemRepository {
    Page<ActivityItem> findAll(ActivityItemFilter filter, int page, int size);
}
