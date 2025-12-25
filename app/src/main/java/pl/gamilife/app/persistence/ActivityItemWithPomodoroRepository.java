package pl.gamilife.app.persistence;

import pl.gamilife.app.dto.ActivityItemWithPomodoroFilter;
import pl.gamilife.app.persistence.view.ActivityItemWithPomodoro;
import pl.gamilife.shared.kernel.architecture.Page;

public interface ActivityItemWithPomodoroRepository {
    Page<ActivityItemWithPomodoro> getActivityItemsWithPomodoro(ActivityItemWithPomodoroFilter filter, int page, int size);
}
