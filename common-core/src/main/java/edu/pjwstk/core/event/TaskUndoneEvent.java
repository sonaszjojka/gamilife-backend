package edu.pjwstk.core.event;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.UUID;

@Getter
@AllArgsConstructor
public class TaskUndoneEvent {
    private UUID userId;
}
