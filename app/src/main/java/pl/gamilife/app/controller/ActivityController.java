package pl.gamilife.app.controller;

import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import pl.gamilife.app.dto.ActivityItemDetails;
import pl.gamilife.app.dto.ActivityItemQueryDto;
import pl.gamilife.app.service.ActivityService;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;
import pl.gamilife.shared.web.util.annotation.CurrentUserTimezone;

import java.time.LocalDate;
import java.time.ZoneId;
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
            @RequestParam(defaultValue = "0") Integer page,
            @RequestParam(defaultValue = "10") Integer size
    ) {
        return ResponseEntity.ok(
                activityService.getAllActivities(new ActivityItemQueryDto(
                        userId,
                        zoneId,
                        title,
                        categoryId,
                        difficultyId,
                        startDate,
                        endDate,
                        page,
                        size
                ))
        );
    }

}
