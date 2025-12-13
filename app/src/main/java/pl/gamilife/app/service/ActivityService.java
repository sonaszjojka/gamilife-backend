package pl.gamilife.app.service;

import pl.gamilife.app.dto.ActivityItemDetails;
import pl.gamilife.app.dto.ActivityItemQueryDto;
import pl.gamilife.shared.kernel.architecture.Page;

public interface ActivityService {

    Page<ActivityItemDetails> getAllActivities(ActivityItemQueryDto dto);
}
