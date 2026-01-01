package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record LevelUpEvent(
        UUID userId,
        int level
) {
}
