package pl.gamilife.api.task.dto;

import java.io.Serializable;
import java.util.UUID;

public record HabitDto(
        UUID userId,
        boolean canBeWorkedOn
) implements Serializable {
}