package pl.gamilife.groupshop.infrastructure.web;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.groupshop.application.deletegroupitem.DeleteGroupItemCommand;
import pl.gamilife.groupshop.application.getgroupitems.GetGroupItemsCommand;
import pl.gamilife.groupshop.application.getgroupitems.GetGroupItemsResult;
import pl.gamilife.groupshop.application.getgroupitems.GetGroupItemsUseCase;
import pl.gamilife.groupshop.domain.model.projection.ApiResponse;
import pl.gamilife.groupshop.application.creategroupitem.CreateGroupItemInShopCommand;
import pl.gamilife.groupshop.application.creategroupitem.CreateGroupItemInShopResult;
import pl.gamilife.groupshop.application.creategroupitem.CreateGroupItemInShopUseCase;
import pl.gamilife.groupshop.application.deletegroupitem.DeleteGroupItemUseCase;
import pl.gamilife.groupshop.application.editgroupitem.EditGroupItemCommand;
import pl.gamilife.groupshop.application.editgroupitem.EditGroupItemResult;
import pl.gamilife.groupshop.application.editgroupitem.EditGroupItemUseCase;
import pl.gamilife.groupshop.infrastructure.web.request.CreateGroupItemRequest;
import pl.gamilife.groupshop.infrastructure.web.request.EditGroupItemRequest;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@AllArgsConstructor
@RestController
@RequestMapping("/api/v1/groups/{groupId}/shop/{shopId}/items")
public class GroupItemController {
    private final CreateGroupItemInShopUseCase createGroupItemInShopUseCase;
    private final DeleteGroupItemUseCase deleteGroupItemUseCase;
    private final EditGroupItemUseCase editGroupItemUseCase;
    private final GetGroupItemsUseCase getGroupItemsUseCase;


    @PostMapping("")
    public ResponseEntity<CreateGroupItemInShopResult> createGroupItemInShop(@PathVariable(name = "groupId") UUID groupId,
                                                                             @PathVariable(name = "shopId") UUID groupShopId,
                                                                             @CurrentUserId UUID currentUserId,
                                                                             @RequestBody @Valid CreateGroupItemRequest request) {

        CreateGroupItemInShopCommand cmd = new CreateGroupItemInShopCommand(
                request.name(),
                request.price(),
                request.isActive(),
                groupShopId,
                groupId,
                currentUserId
        );
        CreateGroupItemInShopResult response = createGroupItemInShopUseCase.execute(cmd);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{itemId}")
    public ResponseEntity<ApiResponse> deleteGroupItemInShop(@PathVariable(name = "groupId") UUID groupId,
                                                             @PathVariable(name = "itemId") UUID groupItemId,
                                                             @CurrentUserId UUID currentUserId) {
        DeleteGroupItemCommand cmd = new DeleteGroupItemCommand(groupItemId, groupId,currentUserId);
        deleteGroupItemUseCase.execute(cmd);
        return ResponseEntity.ok(new ApiResponse("Group item in shop deleted successfully"));
    }

    @PutMapping("/{itemId}")
    public ResponseEntity<EditGroupItemResult> editGroupItemInShop(@PathVariable(name = "groupId") UUID groupId,
                                                                   @PathVariable(name = "itemId") UUID groupItemId,
                                                                   @CurrentUserId UUID currentUserId,
                                                                   @RequestBody @Valid EditGroupItemRequest request) {
        EditGroupItemResult response = editGroupItemUseCase.execute(new EditGroupItemCommand(request.name(), request.price(), request.isActive(), groupItemId, groupId, currentUserId));
        return ResponseEntity.ok(response);
    }

    @GetMapping("")
    public ResponseEntity<Page<GetGroupItemsResult>> getGroupItemsInShop(@PathVariable(name = "groupId") UUID groupId,
                                                             @PathVariable(name = "shopId") UUID shopId,
                                                             @RequestParam(name="isActive") Boolean isActive,
                                                             @RequestParam(name = "page", defaultValue = "0")@Min(0) int page,
                                                             @RequestParam(name = "size", defaultValue = "10")@Min(0) @Max(100) int size) {


        Page<GetGroupItemsResult> response = getGroupItemsUseCase.execute(new GetGroupItemsCommand(groupId, shopId, isActive, page, size));
        return ResponseEntity.ok(response);
    }


}
