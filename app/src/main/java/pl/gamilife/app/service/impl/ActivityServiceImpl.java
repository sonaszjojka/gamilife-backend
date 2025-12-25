package pl.gamilife.app.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.pomodoro.PomodoroApi;
import pl.gamilife.api.pomodoro.dto.PomodoroItemDto;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.ActivityItemDto;
import pl.gamilife.api.task.dto.ActivityItemQuery;
import pl.gamilife.app.dto.ActivityItemDetails;
import pl.gamilife.app.dto.ActivityItemQueryDto;
import pl.gamilife.app.dto.ActivityItemWithPomodoroFilter;
import pl.gamilife.app.dto.ActivityItemWithPomodoroQueryDto;
import pl.gamilife.app.persistence.ActivityItemWithPomodoroRepository;
import pl.gamilife.app.persistence.view.ActivityItemWithPomodoro;
import pl.gamilife.app.service.ActivityService;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.user.api.UserApiImpl;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@AllArgsConstructor
public class ActivityServiceImpl implements ActivityService {

    private final TaskApi taskApi;
    private final PomodoroApi pomodoroApi;
    private final ActivityItemWithPomodoroRepository activityItemWithPomodoroRepository;
    private final UserApiImpl userApi;

    @Override
    public Page<ActivityItemDetails> getAllActivities(ActivityItemQueryDto dto) {
        Page<ActivityItemDto> page = taskApi.getAllActivityItemsFiltered(new ActivityItemQuery(
                dto.userId(),
                dto.zoneId(),
                dto.title(),
                dto.categoryId(),
                dto.difficultyId(),
                dto.startDate(),
                dto.endDate(),
                dto.page(),
                dto.size()
        ));

        List<pl.gamilife.api.pomodoro.dto.ActivityItemDto> activityItemIds = page.content()
                .stream()
                .map(i -> new pl.gamilife.api.pomodoro.dto.ActivityItemDto(
                        i.id(),
                        i.type()
                ))
                .toList();

        Map<UUID, ActivityItemDetails.Pomodoro> map = pomodoroApi.findPomodoroItemsByActivityIds(activityItemIds).stream()
                .collect(Collectors.toMap(
                        PomodoroItemDto::activityId,
                        pi -> new ActivityItemDetails.Pomodoro(
                                pi.pomodoroId(),
                                pi.cyclesRequired(),
                                pi.cyclesCompleted()
                        )));

        return page.map(ai -> new ActivityItemDetails(
                ai.id(),
                ActivityItemDetails.ActivityType.from(ai.type()),
                ai.title(),
                ai.description(),
                ai.categoryId(),
                ai.categoryName(),
                ai.difficultyId(),
                ai.difficultyName(),
                ai.deadlineDate(),
                ai.deadlineTime(),
                ai.cycleLength(),
                ai.currentStreak(),
                ai.longestStreak(),
                switch (ai.status()) {
                    case ALIVE -> ActivityItemDetails.ActivityStatus.ALIVE;
                    case INCOMPLETE -> ActivityItemDetails.ActivityStatus.INCOMPLETE;
                    case DEADLINE_TODAY -> ActivityItemDetails.ActivityStatus.DEADLINE_TODAY;
                    case DEADLINE_MISSED -> ActivityItemDetails.ActivityStatus.DEADLINE_MISSED;
                },
                ai.canBeWorkedOn(),
                map.get(ai.id())
        ));
    }

    @Override
    public Page<ActivityItemDetails> getActivitiesWithPomodoro(ActivityItemWithPomodoroQueryDto dto) {
        ZoneId zoneId = dto.zoneId() == null
                ? userApi.getUserZoneId(dto.userId())
                : dto.zoneId();

        Page<ActivityItemWithPomodoro> page = activityItemWithPomodoroRepository.getActivityItemsWithPomodoro(
                new ActivityItemWithPomodoroFilter(
                        dto.userId(),
                        zoneId,
                        dto.title(),
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
                ai.getPomodoroId() != null ? new ActivityItemDetails.Pomodoro(
                        ai.getPomodoroId(),
                        ai.getCyclesRequired(),
                        ai.getCyclesCompleted()
                ) : null
        ));
    }
}
