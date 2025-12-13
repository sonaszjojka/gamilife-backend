package pl.gamilife.task.application.getusersactivityitems;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.filter.ActivityItemFilter;
import pl.gamilife.task.domain.model.projection.ActivityItem;
import pl.gamilife.task.domain.port.context.UserContext;
import pl.gamilife.task.domain.port.repository.ActivityItemRepository;

import java.time.LocalDateTime;
import java.time.ZoneId;

@Service
@AllArgsConstructor
public class GetUsersActivityItemsUseCaseImpl implements GetUsersActivityItemsUseCase {

    private final ActivityItemRepository activityItemRepository;
    private final UserContext userContext;

    @Override
    public Page<GetUsersActivityItemsResult> execute(GetUsersActivityItemsCommand cmd) {
        ZoneId zoneId = cmd.zoneId() == null
                ? userContext.getCurrentUserTimezone(cmd.userId())
                : cmd.zoneId();

        Page<ActivityItem> result = activityItemRepository.findAll(
                new ActivityItemFilter(
                        cmd.userId(),
                        cmd.title(),
                        cmd.categoryId(),
                        cmd.difficultyId(),
                        cmd.startDate(),
                        cmd.endDate()
                ),
                cmd.page(),
                cmd.size()
        );

        return result.map(ai -> new GetUsersActivityItemsResult(
                ai.getId(),
                ai.getType(),
                ai.getTitle(),
                ai.getDescription(),
                ai.getUserId(),
                ai.getCategoryId(),
                ai.getCategoryName(),
                ai.getDifficultyId(),
                ai.getDifficultyName(),
                ai.getDeadlineDate(),
                ai.getDeadlineTime(),
                switch (ai.calculateCurrentStatus(LocalDateTime.now(zoneId))) {
                    case ALIVE -> GetUsersActivityItemsResult.ActivityStatus.ALIVE;
                    case INCOMPLETE -> GetUsersActivityItemsResult.ActivityStatus.INCOMPLETE;
                    case DEADLINE_TODAY -> GetUsersActivityItemsResult.ActivityStatus.DEADLINE_TODAY;
                    case DEADLINE_MISSED -> GetUsersActivityItemsResult.ActivityStatus.DEADLINE_MISSED;
                }
        ));
    }
}
