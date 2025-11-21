package edu.pjwstk.core.event;

import lombok.Builder;
import lombok.Getter;

import java.util.UUID;

@Getter
@Builder
public class ItemBoughtEvent {
    private UUID userId;
}
