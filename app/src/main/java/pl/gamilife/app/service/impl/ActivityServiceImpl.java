package pl.gamilife.app.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.app.dto.activity.ActivityItemDetails;
import pl.gamilife.app.dto.activity.ActivityItemFilter;
import pl.gamilife.app.dto.activity.ActivityItemQueryDto;
import pl.gamilife.app.persistence.ActivityItemRepository;
import pl.gamilife.app.persistence.view.ActivityItem;
import pl.gamilife.app.service.ActivityService;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.user.api.UserApiImpl;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;

@Service
@AllArgsConstructor
public class ActivityServiceImpl implements ActivityService {

    private final ActivityItemRepository activityItemRepository;
    private final UserApiImpl userApi;

    @Override
    public Page<ActivityItemDetails> getActivities(ActivityItemQueryDto dto) {
        ZoneId zoneId = dto.zoneId() == null
                ? userApi.getUserZoneId(dto.userId())
                : dto.zoneId();

        Page<ActivityItem> page = activityItemRepository.getActivityItemsWithPomodoro(
                new ActivityItemFilter(
                        dto.userId(),
                        zoneId,
                        dto.title(),
                        dto.categoryId(),
                        dto.difficultyId(),
                        dto.startDate(),
                        dto.endDate(),
                        dto.workable(),
                        dto.pomodoro()
                ),
                dto.page(),
                dto.size()
        );

        LocalDateTime currentUserDateTime = LocalDateTime.now(zoneId);
        LocalDate currentUserDate = currentUserDateTime.toLocalDate();

        return page.map(ai -> new ActivityItemDetails(
                ai.getId(),
                ActivityItemDetails.ActivityType.from(ai.getType()),
                ai.getTitle(),
                ai.getDescription(),
                ai.getCategoryId(),
                ai.getCategoryName(),
                ai.getDifficultyId(),
                ai.getDifficultyName(),
                ai.getDeadlineDate(),
                ai.getDeadlineTime(),
                ai.getCycleLength(),
                ai.getCurrentStreak(),
                ai.getLongestStreak(),
                switch (ai.calculateCurrentStatus(currentUserDateTime)) {
                    case ALIVE -> ActivityItemDetails.ActivityStatus.ALIVE;
                    case INCOMPLETE -> ActivityItemDetails.ActivityStatus.INCOMPLETE;
                    case DEADLINE_TODAY -> ActivityItemDetails.ActivityStatus.DEADLINE_TODAY;
                    case DEADLINE_MISSED -> ActivityItemDetails.ActivityStatus.DEADLINE_MISSED;
                },
                ai.canBeWorkedOn(currentUserDate),
                ai.getPomodoroId() != null ? new ActivityItemDetails.PomodoroDto(
                        ai.getPomodoroId(),
                        ai.getCyclesRequired(),
                        ai.getCyclesCompleted()
                ) : null
        ));
    }
}
