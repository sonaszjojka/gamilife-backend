package pl.gamilife.groupshop.api.controller;

import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.groupshop.shared.ApiResponse;
import pl.gamilife.groupshop.usecase.creategroupiteminshop.CreateGroupItemInShopRequest;
import pl.gamilife.groupshop.usecase.creategroupiteminshop.CreateGroupItemInShopResponse;
import pl.gamilife.groupshop.usecase.creategroupiteminshop.CreateGroupItemInShopUseCase;
import pl.gamilife.groupshop.usecase.deletegroupiteminshop.DeleteGroupItemInShopUseCase;
import pl.gamilife.groupshop.usecase.editgroupiteminshop.EditGroupItemInShopRequest;
import pl.gamilife.groupshop.usecase.editgroupiteminshop.EditGroupItemInShopResponse;
import pl.gamilife.groupshop.usecase.editgroupiteminshop.EditGroupItemInShopUseCase;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/shop/{shopId}/items")
public class GroupItemInShopController {
    private final CreateGroupItemInShopUseCase createGroupItemInShopUseCase;
    private final DeleteGroupItemInShopUseCase deleteGroupItemInShopUseCase;
    private final EditGroupItemInShopUseCase editGroupItemInShopUseCase;

    public GroupItemInShopController(CreateGroupItemInShopUseCase createGroupItemInShopUseCase, DeleteGroupItemInShopUseCase deleteGroupItemInShopUseCase, EditGroupItemInShopUseCase editGroupItemInShopUseCase) {
        this.createGroupItemInShopUseCase = createGroupItemInShopUseCase;
        this.deleteGroupItemInShopUseCase = deleteGroupItemInShopUseCase;
        this.editGroupItemInShopUseCase = editGroupItemInShopUseCase;
    }


    @PostMapping("")
    public ResponseEntity<CreateGroupItemInShopResponse> createGroupItemInShop(@PathVariable(name = "groupId") UUID groupId,
                                                                               @PathVariable(name = "shopId") UUID groupShopId,
                                                                               @RequestBody @Valid CreateGroupItemInShopRequest request) {
        CreateGroupItemInShopResponse response = createGroupItemInShopUseCase.execute(request, groupId, groupShopId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping("/{itemId}")
    public ResponseEntity<ApiResponse> deleteGroupItemInShop(@PathVariable(name = "groupId") UUID groupId,
                                                             @PathVariable(name = "itemId") UUID groupItemId) {
        deleteGroupItemInShopUseCase.deleteById(groupItemId, groupId);
        return ResponseEntity.ok(new ApiResponse("Group item in shop deleted successfully"));
    }

    @PutMapping("/{itemId}")
    public ResponseEntity<EditGroupItemInShopResponse> editGroupItemInShop(@PathVariable(name = "groupId") UUID groupId,
                                                                           @PathVariable(name = "itemId") UUID groupItemId,
                                                                           @RequestBody @Valid EditGroupItemInShopRequest request) {
        EditGroupItemInShopResponse response = editGroupItemInShopUseCase.execute(groupItemId, groupId, request);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }


}
