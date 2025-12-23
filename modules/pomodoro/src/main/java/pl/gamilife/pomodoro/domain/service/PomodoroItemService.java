package pl.gamilife.pomodoro.domain.service;

import pl.gamilife.pomodoro.domain.model.PomodoroItem;

import java.time.ZoneId;
import java.util.UUID;

public interface PomodoroItemService {
    PomodoroItem ensureExistsAndBelongsToUser(UUID pomodoroId, UUID currentUserId, ZoneId zoneId);
}
