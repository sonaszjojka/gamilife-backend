package pl.gamilife.app.persistence;

import pl.gamilife.app.dto.activity.ActivityItemFilter;
import pl.gamilife.app.persistence.view.ActivityItem;
import pl.gamilife.shared.kernel.architecture.Page;

public interface ActivityItemRepository {
    Page<ActivityItem> getActivityItemsWithPomodoro(ActivityItemFilter filter, int page, int size);
}
