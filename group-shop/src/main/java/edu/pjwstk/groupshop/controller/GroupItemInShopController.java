package edu.pjwstk.groupshop.controller;
import edu.pjwstk.groupshop.usecase.creategroupiteminshop.CreateGroupItemInShopRequest;
import edu.pjwstk.groupshop.usecase.creategroupiteminshop.CreateGroupItemInShopResponse;
import edu.pjwstk.groupshop.usecase.creategroupiteminshop.CreateGroupItemInShopUseCase;
import edu.pjwstk.groupshop.usecase.deletegroupiteminshop.DeleteGroupItemInShopUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/shop/{shopId}/items")
public class GroupItemInShopController {
    private final CreateGroupItemInShopUseCase createGroupItemInShopUseCase;
    private final DeleteGroupItemInShopUseCase deleteGroupItemInShopUseCase;

    public GroupItemInShopController(CreateGroupItemInShopUseCase createGroupItemInShopUseCase, DeleteGroupItemInShopUseCase deleteGroupItemInShopUseCase){
        this.createGroupItemInShopUseCase = createGroupItemInShopUseCase;
        this.deleteGroupItemInShopUseCase = deleteGroupItemInShopUseCase;
    }


    @PostMapping("")
    public ResponseEntity<CreateGroupItemInShopResponse> createGroupItemInShop(@PathVariable (name="groupId") UUID groupId,
                                                                         @PathVariable (name="shopId") UUID groupShopId,
                                                                         @RequestBody @Valid CreateGroupItemInShopRequest request)
    {
        CreateGroupItemInShopResponse response = createGroupItemInShopUseCase.execute(request, groupId, groupShopId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping("/{itemId}")
    public ResponseEntity<Void> deleteGroupItemInShop(@PathVariable (name="groupId") UUID groupId,
                                                      @PathVariable (name="itemId") UUID groupItemId)
    {
        deleteGroupItemInShopUseCase.deleteById(groupItemId, groupId);
        return ResponseEntity.noContent().build();
    }
}
