package edu.pjwstk.groupshop.usecase.deletegroupiteminshop;

import org.springframework.http.ResponseEntity;

import java.util.UUID;

public interface DeleteGroupItemInShopUseCase {
    void deleteById(UUID groupItemInShopId,UUID groupId);
}
