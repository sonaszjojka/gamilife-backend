package pl.gamilife.app.controller;

import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import pl.gamilife.app.dto.activity.ActivityItemDetails;
import pl.gamilife.app.dto.activity.ActivityItemQueryDto;
import pl.gamilife.app.service.ActivityService;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;
import pl.gamilife.shared.web.util.annotation.CurrentUserTimezone;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Objects;
import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/activities")
public class ActivityController {

    private final ActivityService activityService;

    @GetMapping
    public ResponseEntity<Page<ActivityItemDetails>> getAllActivities(
            @CurrentUserId UUID userId,
            @CurrentUserTimezone ZoneId zoneId,
            @RequestParam(required = false) String title,
            @RequestParam(required = false) Integer categoryId,
            @RequestParam(required = false) Integer difficultyId,
            @RequestParam(required = false) LocalDate startDate,
            @RequestParam(required = false) LocalDate endDate,
            @RequestParam(required = false) Boolean workable,
            @RequestParam(required = false) Boolean pomodoro,
            @RequestParam(required = false) Integer page,
            @RequestParam(required = false) Integer size
    ) {
        int pageNumber = page != null ? page : 0;
        int pageSize = Objects.requireNonNullElseGet(
                size,
                () -> startDate != null && endDate != null ? Integer.MAX_VALUE : 10
        );

        return ResponseEntity.ok(
                activityService.getActivities(new ActivityItemQueryDto(
                        userId,
                        zoneId,
                        title,
                        categoryId,
                        difficultyId,
                        startDate,
                        endDate,
                        workable,
                        pomodoro,
                        pageNumber,
                        pageSize
                ))
        );
    }

}
