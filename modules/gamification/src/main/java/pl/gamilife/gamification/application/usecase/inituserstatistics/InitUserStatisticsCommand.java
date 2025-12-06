package pl.gamilife.gamification.application.usecase.inituserstatistics;

import pl.gamilife.infrastructure.core.architecture.Command;

import java.util.UUID;

public record InitUserStatisticsCommand(UUID userId) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new IllegalArgumentException("userId cannot be null");
        }
    }
}
