package pl.gamilife.groupshop.application.creategroupshopforgroup;
import java.util.UUID;

public record CreateGroupShopForGroupResult(

        UUID groupShopId,
        UUID groupId,
        String name,
        String description

) {
}
