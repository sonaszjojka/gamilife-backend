package pl.gamilife.infrastructure.core.event;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.UUID;

@Getter
@AllArgsConstructor
public class JoinedGroupEvent {
    private UUID userId;
    private boolean isFirstTimeJoin;
}
