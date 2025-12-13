package pl.gamilife.api.pomodoro;


import pl.gamilife.api.pomodoro.dto.ActivityItemDto;
import pl.gamilife.api.pomodoro.dto.PomodoroItemDto;

import java.util.List;

public interface PomodoroApi {
    List<PomodoroItemDto> findPomodoroItemsByActivityIds(List<ActivityItemDto> attachedEntities);
}
