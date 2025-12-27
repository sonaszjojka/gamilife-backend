package pl.gamilife.groupshop.infrastructure.web;

import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.groupshop.application.deletegroupitem.DeleteGroupItemCommand;
import pl.gamilife.groupshop.domain.model.projection.ApiResponse;
import pl.gamilife.groupshop.application.creategroupiteminshop.CreateGroupItemInShopCommand;
import pl.gamilife.groupshop.application.creategroupiteminshop.CreateGroupItemInShopResult;
import pl.gamilife.groupshop.application.creategroupiteminshop.CreateGroupItemInShopUseCase;
import pl.gamilife.groupshop.application.deletegroupitem.DeleteGroupItemUseCase;
import pl.gamilife.groupshop.application.editgroupitem.EditGroupItemCommand;
import pl.gamilife.groupshop.application.editgroupitem.EditGroupItemResult;
import pl.gamilife.groupshop.application.editgroupitem.EditGroupItemUseCase;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/shop/{shopId}/items")
public class GroupItemController {
    private final CreateGroupItemInShopUseCase createGroupItemInShopUseCase;
    private final DeleteGroupItemUseCase deleteGroupItemUseCase;
    private final EditGroupItemUseCase editGroupItemUseCase;

    public GroupItemController(CreateGroupItemInShopUseCase createGroupItemInShopUseCase, DeleteGroupItemUseCase deleteGroupItemUseCase, EditGroupItemUseCase editGroupItemUseCase) {
        this.createGroupItemInShopUseCase = createGroupItemInShopUseCase;
        this.deleteGroupItemUseCase = deleteGroupItemUseCase;
        this.editGroupItemUseCase = editGroupItemUseCase;
    }


    @PostMapping("")
    public ResponseEntity<CreateGroupItemInShopResult> createGroupItemInShop(@PathVariable(name = "groupId") UUID groupId,
                                                                             @PathVariable(name = "shopId") UUID groupShopId,
                                                                             @CurrentUserId UUID currentUserId,
                                                                             @RequestBody @Valid CreateGroupItemInShopCommand request) {

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
        deleteGroupItemUseCase.deleteById(cmd);
        return ResponseEntity.ok(new ApiResponse("Group item in shop deleted successfully"));
    }

    @PutMapping("/{itemId}")
    public ResponseEntity<EditGroupItemResult> editGroupItemInShop(@PathVariable(name = "groupId") UUID groupId,
                                                                   @PathVariable(name = "itemId") UUID groupItemId,
                                                                   @CurrentUserId UUID currentUserId,
                                                                   @RequestBody @Valid EditGroupItemCommand request) {
        EditGroupItemResult response = editGroupItemUseCase.execute(new EditGroupItemCommand(request.name(), request.price(), request.isActive(), groupItemId, groupId, currentUserId));
        return ResponseEntity.ok(response);
    }


}
