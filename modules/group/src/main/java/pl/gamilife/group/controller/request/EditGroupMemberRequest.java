package pl.gamilife.group.controller.request;

import jakarta.validation.constraints.PositiveOrZero;
import lombok.Builder;
import pl.gamilife.group.model.GroupMember;

import java.io.Serializable;

/**
 * DTO for {@link GroupMember}
 */
@Builder
public record EditGroupMemberRequest(

        @PositiveOrZero(message = "Group money must be positive or zero")
        Integer groupMoney,

        @PositiveOrZero(message = "Total earned money must be positive or zero")
        Integer totalEarnedMoney

) implements Serializable {
}