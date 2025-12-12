package pl.gamilife.api.pomodoro;


import pl.gamilife.api.pomodoro.dto.PomodoroItemDto;

import java.util.List;
import java.util.UUID;

public interface PomodoroApi {
    List<PomodoroItemDto> findPomodoroItemsByTaskIds(List<UUID> taskId);

    List<PomodoroItemDto> findPomodoroItemsByHabitIds(List<UUID> taskId);
}
