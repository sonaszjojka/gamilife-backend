package pl.gamilife.shared.kernel.event;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.UUID;

@Getter
@AllArgsConstructor
public class GroupItemPurchasedEvent {
    private UUID userId;
}
