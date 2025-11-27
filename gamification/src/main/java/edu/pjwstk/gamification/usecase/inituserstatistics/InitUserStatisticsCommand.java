package edu.pjwstk.gamification.usecase.inituserstatistics;

import edu.pjwstk.core.Command;

import java.util.UUID;

public record InitUserStatisticsCommand(UUID userId) implements Command {
    @Override
    public void validate() {
        if (userId == null) {
            throw new IllegalArgumentException("userId cannot be null");
        }
    }
}
