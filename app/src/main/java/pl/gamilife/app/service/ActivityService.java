package pl.gamilife.app.service;

import pl.gamilife.app.dto.activity.ActivityItemDetails;
import pl.gamilife.app.dto.activity.ActivityItemQueryDto;
import pl.gamilife.shared.kernel.architecture.Page;

public interface ActivityService {

    Page<ActivityItemDetails> getActivities(ActivityItemQueryDto dto);

}
