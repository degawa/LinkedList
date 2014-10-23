program doublylinkedlist
    use class_DoublyList
    use class_DoublyLink
    implicit none
    
    integer :: i=0
    type(DoublyList) :: mylist
    class(DoublyLink),pointer :: doublyLinkItem

    type(DoublyListIterator),pointer :: itr
    
    !リストへの整数，浮動小数，文字の追加と表示
    !print()で表示できるのはこの3種類の型のみ
    do i=1,10
        call mylist%add(i)
    end do
    call mylist%add(1.1)
    call mylist%add('A')
    call mylist%add('B')
    call mylist%add('C')
    call mylist%printList()
    print *,mylist%size()
    print *,"----------------"
    
    !リストのアイテムを全て初期化して追加を行うテスト
    !call mylist%clear()
    !call mylist%addFirst(0)
    !call mylist%addLast("Z")
    !call mylist%printList()
    !print *,mylist%size()
    !print *,"----------------"
    
    
    !プログラムが落ちる処理のパターン
    !リストのアイテムが1個のときにremoveするとアクセス違反が発生
    !call mylist%clear()
    !call mylist%add(0)
    !print *,mylist%size()
    !!call mylist%removeFirst() !どれを使ってもアクセス違反
    !!call mylist%remove()      !
    !!call mylist%removeLast()  !
    !call mylist%printList()
    !print *,mylist%size()
    !print *,"----------------"

    !Iteratorのテスト
    itr => mylist%iterator()
    call itr%first()
    do while(itr%hasNext())
        doublyLinkItem => toDoublyLink(itr%next())
        call doublyLinkItem%print()
    end do
    print *,"---"
    
    !first()でiteratorがリストの先頭に戻るかをテスト
    call itr%first()
    do while(itr%hasNext())
        doublyLinkItem => toDoublyLink(itr%next())
        call doublyLinkItem%print()
    end do
    
    call mylist%destroyIterator(itr)
    call mylist%clear()

end program doublylinkedlist