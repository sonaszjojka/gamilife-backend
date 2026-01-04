package pl.gamilife.gamification.application.usecase.getuserstatistics;

import pl.gamilife.shared.kernel.architecture.Command;

import java.util.UUID;

public record GetUserStatisticsCommand(
        UUID userId
) implements Command {
}
