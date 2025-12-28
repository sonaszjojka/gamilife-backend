package pl.gamilife.shared.kernel.event;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.UUID;

@Getter
@AllArgsConstructor
public class GroupTaskUndoneEvent {
    private UUID userId;
}
