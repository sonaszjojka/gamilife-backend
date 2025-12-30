package pl.gamilife.shared.kernel.event;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Collection;
import java.util.UUID;

@Getter
@AllArgsConstructor
public class GroupTaskCompletedEvent {
    private Collection<UUID> userIds;
    private boolean rewardGranted;
}
